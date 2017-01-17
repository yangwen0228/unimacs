;;; init-helm.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package helm
  :bind (("M-x"     . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b"   . helm-for-files)
         ("C-c i"   . helm-imenu))
  :config
  (require 'helm-config)
  (helm-mode 1)                       ;turn helm-mode on, don't turn ido-mode on

  (setq helm-autoresize-mode t)

  (setq helm-completing-read-handlers-alist
        '((execute-extended-command . helm-completing-read-symbols)
          (describe-function        . helm-completing-read-symbols)
          (describe-variable        . helm-completing-read-symbols)
          (debug-on-entry           . helm-completing-read-symbols)
          (find-function            . helm-completing-read-symbols)
          (find-tag                 . helm-completing-read-with-cands-in-buffer)
          (ffap-alternate-file . nil)
          (tmm-menubar . nil)
          (dired-do-copy . nil)
          (dired-do-rename . nil)
          (dired-create-directory . nil)
          (find-file . ido)
          (ido-find-file . ido)
          (ido-edit-input . nil)
          (mml-attach-file . ido)
          (read-file-name . ido)
          (read-directory-name . ido)
          (yas-compile-directory . ido)
          (minibuffer-completion-help . nil)
          (minibuffer-complete . nil)
          (wg-load . ido)
          (rgrep . nil)
          (w3m-goto-url . nil)
          (unimacs-copy-file-and-rename-buffer . nil)
          (unimacs-rename-file-and-buffer . nil)
          ))

  ;; helm-do-grep recursive
  ;; don't use this function, but use helm-ag-select-dir instead,
  ;; which is much faster than this. but sometimes that doesn't work,
  ;; so I still keep this.
  (defun helm-do-grep-recursive (&optional non-recursive)
    "Like `helm-do-grep', but greps recursively by default."
    (interactive "P")
    (let* ((current-prefix-arg (not non-recursive))
           (helm-current-prefix-arg non-recursive))
      (call-interactively 'helm-do-grep)))
  ;; (global-set-key (kbd "C-c M-r") 'helm-do-grep-recursive)
  :diminish (helm-mode)
  )

(provide 'init-helm)
;;; init-helm.el ends here
