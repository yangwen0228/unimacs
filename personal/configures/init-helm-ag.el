;;; init-helm-ag.el --- Helm ag configure.
;;; Commentary:
;; comments
;; Please add following configuration if you use helm-ag with the platinum searcher.

;; (custom-set-variables
;;   '(helm-ag-base-command "pt --nocolor --nogroup"))

;; or using ack

;; (custom-set-variables
;;   '(helm-ag-base-command "ack --nocolor --nogroup"))

;;; Code:
(use-package helm-ag
  :bind (("C-c M-r" . helm-ag-dwim))
  :preface
  (defun helm-ag-dwim ()
    "Like `helm-ag', but use projectile root as default directory.

If not in a project, then use file directory. Otherwise, with an optional arg `C-u' select directory, `C-u C-u' select multiple directories."
    (interactive)
    (if current-prefix-arg
        (helm-ag)
      (helm-ag (condition-case nil (projectile-project-root) (error nil)))))

  :config
  (custom-set-variables
   '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
   '(helm-ag-command-option "--all-text")
   '(helm-ag-insert-at-point 'symbol))

  (defun helm-ag--marked-input (escape)
    "Bug: [] should be escaped."
    (when (use-region-p)
      (let ((input (buffer-substring-no-properties (region-beginning) (region-end))))
        (deactivate-mark)
        (if (not escape)
            input
          (setq input (replace-regexp-in-string "\\[" "\\\\[" input))
          (setq input (replace-regexp-in-string "\\]" "\\\\]" input))
          (setq input (replace-regexp-in-string "\(" "\\\\(" input))
          (setq input (replace-regexp-in-string "\)" "\\\\)" input))
          (setq input (replace-regexp-in-string "\{" "\\\\{" input))
          (setq input (replace-regexp-in-string "\}" "\\\\}" input))
          (setq input (replace-regexp-in-string "\\$" "\\\\$" input))))))

  (defun helm-ag--query ()
    (let* ((searched-word (helm-ag--searched-word))
           (marked-word (helm-ag--marked-input t))
           (query (read-from-minibuffer "Pattern: "
                                        (or marked-word searched-word)
                                        nil
                                        nil
                                        'helm-ag--command-history
                                        (helm-aif (symbol-at-point)
                                            (symbol-name it)))))
      (when (string-empty-p query)
        (error "Input is empty!!"))
      (setq helm-ag--last-query query)))

  (defun helm-ag--remove-carrige-returns ()
    (when (helm-ag--windows-p)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "\xd" nil t)
          (replace-match "")))))

  (defun helm-ag--init ()
    (let ((buf-coding buffer-file-coding-system))
      (helm-attrset 'recenter t)
      (with-current-buffer (helm-candidate-buffer 'global)
        (let* ((default-directory (or helm-ag--default-directory
                                      default-directory))
               (cmds (helm-ag--construct-command (helm-attr 'search-this-file)))
               (coding-system-for-read buf-coding)
               (coding-system-for-write buf-coding))
          (setq helm-ag--ignore-case (helm-ag--ignore-case-p cmds helm-ag--last-query)
                helm-ag--last-command cmds)
          (let ((ret (apply 'process-file (car cmds) nil t nil (cdr cmds))))
            (if (zerop (length (buffer-string)))
                (error "No ag output: '%s'" helm-ag--last-query)
              (unless (zerop ret)
                (unless (executable-find (car cmds))
                  (error "'ag' is not installed."))
                (error "Failed: '%s'" helm-ag--last-query))))
          (helm-ag--remove-carrige-returns)
          (helm-ag--recode-windows-file-names)
          (helm-ag--save-current-context)))))

  (defun helm-ag--recode-windows-file-names ()
    (when (helm-gtags--windows-p)
      (save-excursion
        (goto-char (point-min))
        (let (
              ;; (buf-coding buffer-file-coding-system)
              (buf-coding 'utf-8)
              (name-coding file-name-coding-system)
              (beg (point)))
          (while (re-search-forward ".*?:[0-9]+:" nil t)
            (recode-region beg (1- (point)) name-coding buf-coding)
            (end-of-line)
            (setq beg (point)))))))
  )

(provide 'init-helm-ag)
;;; init-helm-ag.el ends here
