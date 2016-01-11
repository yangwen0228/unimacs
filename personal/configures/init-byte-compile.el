;;; init-byte-compile.el --- Summary
;;; Commentary:
;; Auto byte compile configure files.

;;; Code:
(defun my-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil
            t))

(add-hook 'emacs-lisp-mode-hook 'my-remove-elc-on-save)

(add-hook 'kill-emacs-hook 'unimacs-recompile-user-files)

(provide 'init-byte-compile)
;;; init-byte-compile.el ends here