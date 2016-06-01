;;; init-iedit.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package iedit
  :bind ("C-;" . iedit-mode)
  :config
  (define-key iedit-mode-occurrence-keymap (kbd "M-n") 'iedit-next-occurrence)
  (define-key iedit-mode-occurrence-keymap (kbd "M-p") 'iedit-prev-occurrence))

(provide 'init-iedit)
;;; init-iedit.el ends here