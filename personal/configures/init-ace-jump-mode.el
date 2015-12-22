;;; init-ace-jump-mode.el --- Very fantistic minor mode for jumping to char.
;;; Commentary:
;; comments

;;; Code:
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

(setq ace-jump-word-mode-use-query-char nil)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-char-mode)

;; If use evil mode:
(eval-after-load "evil" '(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-char-mode))

(provide 'init-ace-jump-mode)
;;; init-ace-jump-mode.el ends here
