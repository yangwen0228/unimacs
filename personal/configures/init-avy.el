;;; init-avy.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(require 'avy)
(global-set-key (kbd "C-:") 'avy-goto-char)

;; If use evil mode:
(eval-after-load "evil" '(define-key evil-normal-state-map (kbd "SPC") 'avy-goto-char))


(provide 'init-avy)
;;; init-avy.el ends here