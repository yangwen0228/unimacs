;;; init-avy.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package avy
  :bind (("M-g j" . avy-goto-char)
         ("M-g k" . avy-goto-char-2)
         ("M-g s" . avy-goto-word-or-subword-1))
  :config
  )

(provide 'init-avy)
;;; init-avy.el ends here