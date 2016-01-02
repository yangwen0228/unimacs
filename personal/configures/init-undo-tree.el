;;; init-undo-tree.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package undo-tree
  :bind (("C-x u" . undo-tree-visualize)
         ("C-_"   . undo-tree-undo)
         ("C--"   . undo-tree-redo))
  )

(provide 'init-undo-tree)
;;; init-undo-tree.el ends here
