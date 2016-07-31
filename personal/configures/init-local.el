;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(add-hook 'tcl-mode-hook 'modify-tcl-syntax-table t nil)
(defun modify-tcl-syntax-table ()
  (modify-syntax-entry ?$ "-")
  (modify-syntax-entry ?. "-")
  (modify-syntax-entry ?= "-")
  (modify-syntax-entry ?+ "-")
  (modify-syntax-entry ?- "-")
  (modify-syntax-entry ?* "-")
  (modify-syntax-entry ?/ "-")
  (modify-syntax-entry ?: "-")
  (modify-syntax-entry ?_ "w")
  )
(global-hl-line-mode 1)
(set-face-attribute hl-line-face nil :underline nil)
(provide 'init-local)
