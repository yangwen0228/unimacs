;;; init-vc.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package diff-hl
  :defer 0
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode))

(provide 'init-vc)
;;; init-vc.el ends here
