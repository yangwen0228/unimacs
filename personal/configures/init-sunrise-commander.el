;;; init-sunrise-commander.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package sunrise-commander
  :bind (("C-c x" . sunrise)
         ("C-c X" . sunrise-cd))
  :mode (("\\.srvm\\'" . sr-virtual-mode))
  :config
  (use-package sunrise-x-tree)
  (use-package sunrise-x-tabs)
  (use-package sunrise-x-popviewer)
  (use-package sunrise-x-modeline)
  (use-package sunrise-x-mirror)
  (use-package sunrise-x-loop)
  (use-package sunrise-x-checkpoints)
  (use-package sunrise-x-buttons)
  )

(provide 'init-sunrise-commander)
;;; init-sunrise-commander.el ends here
