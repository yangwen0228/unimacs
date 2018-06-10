;;; init-guide-key.el --- Summary
;;; Commentary:
;; Hint key bindings, useful for beginners.

;;; Code:
(use-package guide-key
  :init
  (setq guide-key/guide-key-sequence t)
  (setq guide-key/idle-delay 5.0)
  :config
  (guide-key-mode 1)
  :diminish guide-key-mode)

(provide 'init-guide-key)
;;; init-guide-key.el ends here