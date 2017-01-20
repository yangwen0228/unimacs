;;; init-helm.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package helm
  :bind (("M-x"     . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b"   . helm-multi-files)
         ("C-c i"   . helm-imenu))
  :config
  (require 'helm-config)
  (helm-mode 1)                       ;turn helm-mode on, don't turn ido-mode on

  (setq helm-autoresize-mode t
        helm-ff-newfile-prompt-p nil
        helm-ff-skip-boring-files t)
  ;; Overwrite: don't need "." and "..".
  (defun helm-ff-directory-files (directory &optional full)
    "List contents of DIRECTORY.
Argument FULL mean absolute path.
It is same as `directory-files' but always returns the
dotted filename '.' and '..' even on root directories in Windows
systems."
    (setq directory (file-name-as-directory
                     (expand-file-name directory)))
    (let* (file-error
           (ls   (condition-case err
                     (directory-files
                      directory full directory-files-no-dot-files-regexp)
                   ;; Handle file-error from here for Windows
                   ;; because predicates like `file-readable-p' and friends
                   ;; seem broken on emacs for Windows systems (always returns t).
                   ;; This should never be called on GNU/Linux/Unix
                   ;; as the error is properly intercepted in
                   ;; `helm-find-files-get-candidates' by `file-readable-p'.
                   (file-error
                    (prog1
                        (list (format "%s:%s"
                                      (car err)
                                      (mapconcat 'identity (cdr err) " ")))
                      (setq file-error t)))))
           (dot  (concat directory "."))
           (dot2 (concat directory "..")))
      (puthash directory (+ (length ls) 2) helm-ff--directory-files-hash)
      ;; (append (and (not file-error) (list dot dot2)) ls) ; origin
      (append (and (not file-error) (list dot)) ls)))

  (setq helm-completing-read-handlers-alist
        '((execute-extended-command . helm-completing-read-symbols)
          (describe-function        . helm-completing-read-symbols)
          (describe-variable        . helm-completing-read-symbols)
          (debug-on-entry           . helm-completing-read-symbols)
          (find-function            . helm-completing-read-symbols)
          (find-tag                 . helm-completing-read-with-cands-in-buffer)
          (find-file                . helm-completing-read-default-1)
          (ido-find-file            . helm-completing-read-default-1)
          (mml-attach-file          . helm-completing-read-default-1)
          (read-file-name           . helm-completing-read-default-1)
          (read-directory-name      . helm-completing-read-default-1)
          (yas-compile-directory    . helm-completing-read-default-1)
          (ffap-alternate-file      . helm-completing-read-default-1)
          (tmm-menubar                . nil)
          (dired-do-copy              . nil)
          (dired-do-rename            . nil)
          (dired-create-directory     . nil)
          (ido-edit-input             . nil)
          (minibuffer-completion-help . nil)
          (minibuffer-complete        . nil)
          (rgrep                      . nil)
          (w3m-goto-url               . nil)
          ))

  :diminish (helm-mode)
  )

(provide 'init-helm)
;;; init-helm.el ends here
