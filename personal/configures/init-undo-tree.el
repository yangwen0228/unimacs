;;; init-undo-tree.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package undo-tree
  :bind (("C-x u" . undo-tree-visualize)
         ("C-_"   . undo-tree-undo)
         ("C--"   . undo-tree-redo))
  :config
  ;; Bugfix: the cursor jump twice
  (defun undo-tree-undo (&optional arg)
    "Undo changes.
Repeat this command to undo more changes.
A numeric ARG serves as a repeat count.

In Transient Mark mode when the mark is active, only undo changes
within the current region. Similarly, when not in Transient Mark
mode, just \\[universal-argument] as an argument limits undo to
changes within the current region."
    (interactive "*P")
    ;; throw error if undo is disabled in buffer
    (when (eq buffer-undo-list t)
      (user-error "No undo information in this buffer"))
    (undo-tree-undo-1 arg)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Bugfix: the cursor jump twice
    (undo-tree-redo-1 arg)
    (undo-tree-undo-1 arg)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; inform user if at branch point
    (when (> (undo-tree-num-branches) 1) (message "Undo branch point!")))
  )

(provide 'init-undo-tree)
;;; init-undo-tree.el ends here
