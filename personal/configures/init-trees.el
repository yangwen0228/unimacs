;;; init-trees.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package ztree
  :commands (ztree-diff ztree-dir))

(use-package neotree
  ;; TODO:
  ;; use the auto update projectile dir from projectile-speedbar
  ;; use search recursive from project-explorer
  :commands neotree neotree-toggle
  :init
  (global-set-key [f8] 'neotree-toggle)
  (global-set-key [f5] 'neotree-projectile-action)
  (global-set-key [f6] 'neotree-dir))

(provide 'init-trees)
;;; init-trees.el ends here