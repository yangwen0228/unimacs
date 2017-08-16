;;; init-helm-gtags.el --- configure for helm gtags
;;; Commentary:
;; comments

;;; Code:
(use-package helm-gtags
  :diminish helm-gtags-mode
  :init
  (add-hook 'prog-mode-hook (lambda () (helm-gtags-mode t)))
  :commands helm-gtags-create-tags helm-gtags-update-entire-tags helm-gtags-delete-tags
  :bind-keymap ("C-c g" . helm-gtags-mode-map)
  :bind (("M-t"     . helm-gtags-find-tag-adapter)
         ("M-r"     . helm-gtags-find-rtag-adapter)
         ("C-c M-g" . helm-gtags-find-pattern)
         :map helm-gtags-mode-map
         ("C-c g c" . helm-gtags-create-tags)
         ("C-c g d" . helm-gtags-delete-tags)
         ("C-c g u" . helm-gtags-update-entire-tags))
  :preface
  (defun helm-gtags-update-entire-tags ()
    "Update entire project tags."
    (interactive)
    (let* ((how-to 'entire-update)
           (current-time (float-time (current-time)))
           (cmds (helm-gtags--update-tags-command how-to))
           (proc (apply #'start-file-process "helm-gtags-update-tag" nil cmds)))
      (if (not proc)
          (message "Failed: %s" (string-join cmds " "))
        (set-process-sentinel proc (helm-gtags--make-gtags-sentinel 'update))
        (setq helm-gtags--last-update-time current-time))))

  :config
  (setq helm-gtags-path-style             'root
        helm-gtags-update-interval-second nil
        helm-gtags-ignore-case            nil
        helm-gtags-read-only              nil
        helm-gtags-preselect              t
        helm-gtags-auto-update            t
        helm-gtags-pulse-at-cursor        t
        helm-gtags-cache-select-result    t
        helm-gtags-cache-max-result-size  (* 10 1024 1024))
  ;; bug: SPC -> minibuffer-complete-word. Use a method like helm-ag.
  (defun helm-gtags--read-tagname (type &optional default-tagname)
    (let ((tagname (helm-gtags--token-at-point type))
          (prompt (assoc-default type helm-gtags--prompt-alist))
          (comp-func (assoc-default type helm-gtags-comp-func-alist)))
      (if (and tagname helm-gtags-use-input-at-cursor)
          tagname
        (when (and (not tagname) default-tagname)
          (setq tagname default-tagname))
        (when tagname
          (setq prompt (format "%s(default \"%s\") " prompt tagname)))
        (let ((completion-ignore-case helm-gtags-ignore-case)
              (completing-read-function 'completing-read-default))
          (if (and helm-gtags-direct-helm-completing (memq type '(tag rtag symbol find-file)))
              (helm-comp-read prompt comp-func
                              :history 'helm-gtags--completing-history
                              :exec-when-only-one t
                              :default tagname)
            (read-from-minibuffer "Pattern: "
                                  tagname
                                  nil
                                  nil
                                  'helm-gtags--completing-history
                                  (helm-aif (symbol-at-point)
                                      (symbol-name it))))))))

  ;; filename encoding is 'GBK, content is utf-8
  (defun helm-gtags--recode-windows-file-names ()
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
            (setq beg (point)))
          (if (= beg 1)           ; gtags-file -> no ":" output.
              (recode-region (point-min) (point-max) name-coding buf-coding))
          ))))

  (defun helm-gtags-find-tag-adapter (tag)
    "Workaround for tcl rtags: TAG: choose function according to major mode."
    (interactive (list (helm-gtags--read-tagname 'tag)))
    (cond
     ((equal major-mode 'tcl-mode)
      (helm-gtags-find-tag-for-tcl tag))
     (t (helm-gtags-find-tag tag))))

  (defun helm-gtags-find-tag-for-tcl (tag)
    "TODO: Workaround for ctags rtags: Use TAG to find all matches, then filter procs."
    (let* ((ns-tag (s-chop-prefix "::" (helm-gtags-get-tcl-tag-with-ns)))
           (filter-pattern (format "\\(?:proc\\s-+[^ \t]*\\<%s\\s-\\)\\|\\(?:\\(?:\\(?:\\s-\\<variable\\>\\)\\|\\(?:\\<def.*Var\\>\\)\\)\\s-+[^ \t]*\\<%s\\>\\)\\|\\(?:\\(?:::\\)?\\(?:oo::\\)?class\\s-+create\\s-+[^ \t]*\\<%s\\s-\\)\\|\\(?:\\<method\\s-+\\<%s\\s-\\)\\|\\(?:\\(?:::\\)?\\(?:itcl::\\)?class\\s-+[^ \t]*\\<%s\\s-\\)\\|\\(?:\\(?:::\\)?\\(?:itcl::\\)body\\s-+[^ \t]*\\<%s\\s-\\)" ns-tag tag ns-tag tag ns-tag tag))
           (full-pattern (format "<<<<%s>>>>\\<%s\\>" filter-pattern tag)))
      (helm-gtags--common '(helm-source-gtags-pattern) full-pattern)))

  (defun helm-gtags-find-rtag-adapter (tag)
    "Workaround for tcl rtags: TAG: choose function according to major mode."
    (interactive (list (helm-gtags--read-tagname 'rtag)))
    (cond
     ((equal major-mode 'tcl-mode)
      (helm-gtags-find-rtag-for-tcl tag))
     (t (helm-gtags-find-tag tag))))

  (defun helm-gtags-find-rtag-for-tcl (tag)
    "TODO: Workaround for ctags rtags: Use TAG to find all matches, then filter procs."
    (let* ((filter-pattern "^\\(?:proc\\)\\|\\(?:\\s-\\<variable\\>\\)\\|\\(?:\\<def.*Var\\>\\)\\|\\(?:\\<declareVars\\>\\)\\|\\(?:#\\)\\|\\(?:\\s-method\\)\\|\\(?:\\(?:::\\)?\\(?:oo::\\)?class\\)\\|\\(\\(?:::\\)?\\(?:itcl::\\)class\\)\\|\\(\\(?:::\\)?\\(itcl::\\)?body\\)")
           (full-pattern (format "<<<<%s>>>>\\<%s\\>" filter-pattern tag)))
      (helm-gtags--common '(helm-source-gtags-pattern) full-pattern)))

  (defun helm-gtags-get-tcl-tag-with-ns ()
    "Get tag name with namespace, like ::ns::test."
    (let (beg end)
      (and (let ((s (car (syntax-after (point)))))
             (and s (or (= s 1) (= s 2) (= s 3)))) ; 1 . 2 w 3 _
           (save-excursion
             (skip-syntax-backward "w_.")
             (setq beg (point))
             (skip-syntax-forward "w_.")
             (setq end (point)))
           (buffer-substring-no-properties beg end))))

  (defun helm-gtags--exec-global-command (type input &optional detail)
    (let* ((args (helm-gtags--construct-command type input))
           (pattern (car (last args))))
      ;; Modified: remove user added pattern part. (^proc) ...
      (when (eq type 'pattern)
        (when (string-match "^<<<<.*>>>>\\(.*\\)$" (substring-no-properties pattern))
          (setf (car (last args)) (match-string 1 pattern))))
      (helm-gtags--find-tag-directory)
      (helm-gtags--save-current-context)
      (let (
            ;; (buf-coding buffer-file-coding-system)
            (buf-coding 'utf-8))
        (with-current-buffer (helm-candidate-buffer 'global)
          (let ((default-directory (helm-gtags--base-directory))
                (input (car (last args)))
                (coding-system-for-read buf-coding)
                (coding-system-for-write buf-coding))
            (unless (zerop (apply #'process-file "global" nil '(t nil) nil args))
              (error (format "%s: not found" input)))
            ;; --path options does not support searching under GTAGSLIBPATH
            (when (eq type 'find-file)
              (helm-gtags--print-path-in-gtagslibpath args))
            ;; Modified: add filter for tcl definition and reference.
            (when (eq type 'pattern)
              (when (string-match "^<<<<\\(.*?\\)>>>>" (substring-no-properties pattern))
                (helm-gtags--filter-candidates (match-string-no-properties 1 pattern))))
            (helm-gtags--remove-carrige-returns)
            (helm-gtags--recode-windows-file-names)
            (when detail
              (helm-gtags--show-detail)))))))

  (defun helm-gtags--filter-candidates (pattern)
    (when (helm-gtags--windows-p)
      (save-excursion
        (goto-char (point-min))
        (if (string-prefix-p "^" pattern)
            ;; reference:
            (progn
              (setq pattern (substring pattern 1))
              (while (re-search-forward (concat "^.*?:[0-9]+:\\s-*" pattern) nil t)
                (delete-region (line-beginning-position) (line-end-position))))
          ;; definition:
          (let ((cur (point-min)))
            (while (re-search-forward (concat "^.*?:[0-9]+:\\s-*" pattern) nil t)
              (unless (= 1 (line-beginning-position))
                (delete-region cur (1- (line-beginning-position))))
              (setq cur (line-end-position))
              (goto-char (1+ cur)))
            (delete-region cur (point-max)))))))

  (defadvice helm-gtags-find-pattern (before helm-gtags-find-pattern activate)
    "Ignore case when use pattern to search."
    (setq helm-gtags-ignore-case t))
  (defadvice helm-gtags-find-pattern (after helm-gtags-find-pattern activate)
    "Don't ignore case when find definition and reference."
    (setq helm-gtags-ignore-case nil))

  (setq helm-gtags-default-label "ctags")
  (add-hook 'prog-mode-hook
            (lambda ()
              (set (make-local-variable 'helm-gtags-default-label)
                   (if (member major-mode
                               '(c-mode c++-mode objc-mode java-mode))
                       "default"
                     "ctags"))))

  (defun helm-gtags--update-tags-command (how-to)
    "Override the original function."
    (let ((gtagslabel (concat "--gtagslabel=" helm-gtags-default-label)))
      (cl-case how-to
        (entire-update (list "global" gtagslabel "-u"))
        (generate-other-directory (list "global" gtagslabel (helm-gtags--read-tag-directory)))
        (single-update (list "global" gtagslabel "--single-update" (helm-gtags--real-file-name))))))

  (defun helm-gtags-edit ()
    (interactive)
    (helm-exit-and-execute-action 'helm-gtags--edit))

  (defun helm-gtags--edit (_candidate)
    (let ((default-directory (helm-gtags--base-directory)))
      (with-current-buffer (get-buffer-create "*helm-gtags-edit*")
        (erase-buffer)
        (let (buf-content)
          (with-current-buffer (get-buffer "*helm gtags*")
            (goto-char (point-min))
            (forward-line 1)
            (let* ((body-start (point))
                   (marked-lines (cl-loop for ov in (overlays-in body-start (point-max))
                                          when (eq 'helm-visible-mark (overlay-get ov 'face))
                                          return (helm-marked-candidates))))
              (if (not marked-lines)
                  (setq buf-content (buffer-substring-no-properties
                                     body-start (point-max)))
                (setq buf-content (concat (mapconcat 'identity marked-lines "\n") "\n")))))
          (insert buf-content)
          (add-text-properties (point-min) (point-max)
                               '(read-only t rear-nonsticky t front-sticky t))
          (let ((inhibit-read-only t))
            (setq header-line-format
                  (format "[%s] C-c C-c: Commit, C-c C-k: Abort"
                          (abbreviate-file-name default-directory)))
            (goto-char (point-min))
            (while (re-search-forward "^\\(\\(?:[^:]+:\\)\\{1,2\\}\\)\\(.*\\)$" nil t)
              (let ((file-line-begin (match-beginning 1))
                    (file-line-end (match-end 1))
                    (body-begin (match-beginning 2))
                    (body-end (match-end 2)))
                (add-text-properties file-line-begin file-line-end
                                     '(face font-lock-function-name-face
                                            intangible t))
                (remove-text-properties body-begin body-end '(read-only t))
                (set-text-properties body-end (1+ body-end)
                                     '(read-only t rear-nonsticky t))))))))
    (other-window 1)
    (switch-to-buffer (get-buffer "*helm-gtags-edit*"))
    (goto-char (point-min))
    (setq next-error-function 'compilation-next-error-function)
    (setq-local compilation-locs (make-hash-table :test 'equal :weakness 'value))
    (use-local-map helm-gtags-edit-map))

  (defun helm-gtags--edit-commit ()
    (interactive)
    (goto-char (point-min))
    (let ((read-only-files 0)
          (default-directory (helm-gtags--base-directory))
          (line-deletes (make-hash-table :test #'equal)))
      (while (re-search-forward "^\\([^:]+\\):\\([1-9][0-9]*\\):\\(.*\\)$" nil t)
        (let* ((file (match-string-no-properties 1))
               (line (string-to-number (match-string-no-properties 2)))
               (body (match-string-no-properties 3))
               (ovs (overlays-at (line-beginning-position))))
          (with-current-buffer (find-file-noselect file)
            (if buffer-read-only
                (cl-incf read-only-files)
              (goto-char (point-min))
              (let ((deleted-lines (gethash file line-deletes 0))
                    (deleted (and ovs (overlay-get (car ovs) 'helm-gtags-deleted))))
                (forward-line (- line 1 deleted-lines))
                (delete-region (line-beginning-position) (line-end-position))
                (if (not deleted)
                    (insert body)
                  (let ((beg (point)))
                    (forward-line 1)
                    (delete-region beg (point))
                    (puthash file (1+ deleted-lines) line-deletes)))
                (save-buffer))))))
      ;; TODO
      ;; (select-window helm-gtags--original-window)
      (kill-buffer (get-buffer "*helm-gatgs-edit*"))
      (if (not (zerop read-only-files))
          (message "%d files are read-only and not editable." read-only-files)
        (message "Success update"))))

  (defun helm-gtags--edit-abort ()
    (interactive)
    (when (y-or-n-p "Discard changes ?")
      ;; TODO
      ;; (select-window helm-gtags--original-window)
      (kill-buffer (get-buffer "*helm-gatgs-edit*"))
      (message "Abort edit")))

  (defface helm-gtags-edit-deleted-line
    '((t (:inherit font-lock-comment-face :strike-through t)))
    "Face of deleted line in edit mode."
    :group 'helm-gtags)

  (defun helm-gtags--mark-line-deleted ()
    (interactive)
    (let* ((beg (line-beginning-position))
           (end (line-end-position))
           (ov (make-overlay beg end)))
      (overlay-put ov 'face 'helm-gtags-edit-deleted-line)
      (overlay-put ov 'helm-gtags-deleted t)))

  (defun helm-gtags--unmark ()
    (interactive)
    (dolist (ov (overlays-in (line-beginning-position) (line-end-position)))
      (when (overlay-get ov 'helm-gtags-deleted)
        (delete-overlay ov))))

  ;; (setq helm-gtags-mode-map
  ;;       (let ((map (make-sparse-keymap)))
  ;;         (set-keymap-parent map helm-map)
  ;;         (define-key map (kbd "C-c C-e") 'helm-gtags-edit)
  ;;         map))
  (bind-key "C-c C-e" 'helm-gtags-edit helm-map)

  (setq helm-gtags-edit-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "C-c C-c") 'helm-gtags--edit-commit)
          (define-key map (kbd "C-c C-k") 'helm-gtags--edit-abort)
          (define-key map (kbd "C-c C-d") 'helm-gtags--mark-line-deleted)
          (define-key map (kbd "C-c C-u") 'helm-gtags--unmark)
          map))
  )

(provide 'init-helm-gtags)
;;; init-helm-gtags.el ends here