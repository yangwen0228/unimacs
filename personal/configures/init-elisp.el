;;; init-elisp.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package elisp-mode
  :ensure nil
  :init
  (defun elisp-mode-hooks ()
    "lisp-mode-hooks"
    (when (require 'eldoc nil t)
      (setq eldoc-idle-delay 0.2)
      (setq eldoc-echo-area-use-multiline-p t)
      (turn-on-eldoc-mode)))

  (add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)
  )

(provide 'init-elisp)
;;; init-elisp.el ends here
