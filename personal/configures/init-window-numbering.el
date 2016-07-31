;;; init-window-numbering.el --- number the window and use alt + num to jump
;;; Commentary:
;; comments

;;; Code:

;; Enable window-numbering-mode and use M-1 through M-0 to navigate.
;;
;; If you want to affect the numbers, use window-numbering-before-hook or
;; window-numbering-assign-func.
;; For instance, to always assign the calculator window the number 9, add the
;; following to your .emacs:
;;
(use-package window-numbering
  :init
  (window-numbering-mode 1)
  (setq window-numbering-assign-func
        (lambda () (when (equal (buffer-name) "*Calculator*") 9)))
  )
(provide 'init-window-numbering)
;;; init-window-numbering.el ends here