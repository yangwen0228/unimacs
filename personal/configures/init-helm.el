(require 'helm-config)
;(helm-mode 1)

(setq helm-completing-read-handlers-alist
      '((describe-function . ido)
        (describe-variable . ido)
        (debug-on-entry . helm-completing-read-symbols)
        (find-function . helm-completing-read-symbols)
        (find-tag . helm-completing-read-with-cands-in-buffer)
        (ffap-alternate-file . nil)
        (tmm-menubar . nil)
        (dired-do-copy . nil)
        (dired-do-rename . nil)
        (dired-create-directory . nil)
        (find-file . ido)
        (copy-file-and-rename-buffer . nil)
        (rename-file-and-buffer . nil)
        (w3m-goto-url . nil)
        (ido-find-file . nil)
        (ido-edit-input . nil)
        (mml-attach-file . ido)
        (read-file-name . nil)
        (yas/compile-directory . ido)
        (execute-extended-command . ido)
        (minibuffer-completion-help . nil)
        (minibuffer-complete . nil)
        (wg-load . ido)
        (rgrep . nil)
        (read-directory-name . ido)
        ))

;; helm-do-grep recursive
(defun helm-do-grep-recursive (&optional non-recursive)
  "Like `helm-do-grep', but greps recursively by default."
  (interactive "P")
  (let* ((current-prefix-arg (not non-recursive))
         (helm-current-prefix-arg non-recursive))
    (call-interactively 'helm-do-grep)))
;; Don't use this function, but use helm-ag-select-dir instead,
;; which is much faster than this.
;; (global-set-key (kbd "C-c M-r") 'helm-do-grep-recursive)


(if *emacs24*
    (progn
      (global-set-key (kbd "M-x")     'helm-M-x)
      (global-set-key (kbd "C-x C-o") 'helm-find-files)
      (global-set-key (kbd "C-c f")   'helm-for-files)
      (global-set-key (kbd "C-c i")   'helm-imenu)
      )
  (global-set-key (kbd "C-x C-o") 'ffap)
  )

(provide 'init-helm)
