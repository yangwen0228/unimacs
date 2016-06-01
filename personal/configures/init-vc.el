;;; init-vc.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package diff-hl
  :init
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(provide 'init-vc)
;;; init-vc.el ends here
