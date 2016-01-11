(use-package helm
  :bind (("M-x"     . helm-M-x)
         ("C-x C-o" . helm-find-files)
         ("C-c f"   . helm-for-files)
         ("C-c i"   . helm-imenu))
  :config
  (require 'helm-config)
  (helm-mode 1)                           ;turn helm-mode on, don't turn ido-mode on
  (helm-autoresize-mode 0)

  (setq helm-completing-read-handlers-alist
        '((describe-function . helm-completing-read-symbols)
          (describe-variable . helm-completing-read-symbols)
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
  ;; Don't use this function, but use helm-ag-select-dir instead,
  ;; which is much faster than this. But sometimes that doesn't work,
  ;; so I still keep this.
  (defun helm-do-grep-recursive (&optional non-recursive)
    "Like `helm-do-grep', but greps recursively by default."
    (interactive "P")
    (let* ((current-prefix-arg (not non-recursive))
           (helm-current-prefix-arg non-recursive))
      (call-interactively 'helm-do-grep)))
  ;; (global-set-key (kbd "C-c M-r") 'helm-do-grep-recursive)


  )

(provide 'init-helm)
