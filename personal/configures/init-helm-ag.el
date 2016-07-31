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
  :bind (("C-c M-c" . helm-ag)
         ("C-c M-r" . helm-ag-select-directory))
  :preface
  ;; helm-ag input search directory.
  (defun helm-ag-select-directory ()
    "Like `helm-ag', but ag from selected dir."
    (interactive)
    (let ((default-directory
            (file-name-as-directory (read-directory-name "Search directory: " nil nil t))))
      (helm-ag default-directory)
      ))
  :config
  (custom-set-variables
   '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
   '(helm-ag-command-option "--all-text")
   '(helm-ag-insert-at-point 'symbol))

  (defun helm-ag-projectile ()
    (interactive)
    (helm-ag (projectile-project-root)))

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
          (helm-ag--save-current-context)))))

  )

(provide 'init-helm-ag)
;;; init-helm-ag.el ends here
