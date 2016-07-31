;;; init-tcl-hm-mode.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package tcl-hm-mode
  :ensure nil
  :mode (("\\.tm\\'"   . tcl-mode)
         ("\\.tcl\\'"  . tcl-hm-mode)
         ("\\.test\\'" . tcl-hm-mode)
         ("\\.wgt\\'"  . tcl-hm-mode)
         ("\\.msg\\'"  . tcl-hm-mode))
  :config
  (use-package tcl-hm-eldoc
    :ensure nil
    :init (add-hook 'tcl-mode-hook 'tcl-hm-eldoc))
  (defun tcl-hm-macro2tcl ()
    (interactive)
    (let* ((beg (if (region-active-p)
                    (region-beginning)
                  (line-beginning-position)))
           (end (if (region-active-p)
                    (region-end)
                  (line-end-position))))
      (save-excursion
        (replace-regexp "(\\|," " " nil beg end)
        (replace-regexp ")" "" nil beg end))
      ))
  (define-key tcl-hm-mode-map "{"        'tcl-electric-char)
  (define-key tcl-hm-mode-map "}"        'tcl-electric-brace)
  (define-key tcl-hm-mode-map "["        'tcl-electric-char)
  (define-key tcl-hm-mode-map "]"        'tcl-electric-char)
  (define-key tcl-hm-mode-map ";"        'tcl-electric-char)
  (define-key tcl-hm-mode-map "#"        'tcl-electric-hash) ;Remove?  -stef
  (define-key tcl-hm-mode-map "\t"       'tcl-indent-command)
  (define-key tcl-hm-mode-map "\M-\C-x"  'tcl-eval-defun)
  (define-key tcl-hm-mode-map "\C-c\C-j" 'tcl-help-on-hm-word)
  (define-key tcl-hm-mode-map "\C-c\C-v" 'tcl-eval-defun)
  (define-key tcl-hm-mode-map "\C-c\C-f" 'tcl-load-file)
  (define-key tcl-hm-mode-map "\C-c\C-t" 'inferior-tcl)
  (define-key tcl-hm-mode-map "\C-c\C-x" 'tcl-eval-region)

  (add-to-list 'tcl-help-directory-list
               "C/Program Files (x86)/TclPro1.4/lib/tclX8.3/help/tcl/")
  (add-to-list 'tcl-help-directory-list
               "C:/Program Files (x86)/TclPro1.4/lib/tkX8.3/help/tk/")
  (add-to-list 'tcl-hm-help-directory-list
               "C:/Program Files/Altair/13.0/hw/tcl/hwt/docs")
  (add-to-list 'tcl-hm-help-directory-list
               "C:/Program Files/Altair/13.0/help/hwdref/")

  (modify-syntax-entry ?* "w" tcl-mode-syntax-table)
  (modify-syntax-entry ?_ "w" tcl-mode-syntax-table)
  (modify-syntax-entry ?: "." tcl-mode-syntax-table)
  (modify-syntax-entry ?| "_" tcl-mode-syntax-table)
  (modify-syntax-entry ?, "_" tcl-mode-syntax-table)
  (modify-syntax-entry ?! "." tcl-mode-syntax-table)
  (modify-syntax-entry ?$ "." tcl-mode-syntax-table)
  (modify-syntax-entry ?. "." tcl-mode-syntax-table)
  (modify-syntax-entry ?+ "." tcl-mode-syntax-table)
  (modify-syntax-entry ?- "." tcl-mode-syntax-table)
  (modify-syntax-entry ?/ "." tcl-mode-syntax-table)
  (modify-syntax-entry ?= "." tcl-mode-syntax-table)

  (setq tcl-imenu-generic-expression
        `(("Variables" ,(concat
                         "^\\s-*variable[ \t]+"    ; definition
                         "\\([-A-Za-z0-9_:+*]+\\)" ; variable name
                         "[ \t]+[^ \t]+"           ; value
                         ) 1)
          ("Procs" ,(concat
                     (concat "^\\s-*" (regexp-opt `("proc")) "[ \t]+") ; definition
                     "\\([-A-Za-z0-9_:+*]+\\)"                         ; proc name
                     ) 1)
          ("DefMethods" ,(concat
                          (concat "^\\s-*" (regexp-opt `("method"
                                                         "public method"
                                                         "protected method"
                                                         "private method"))
                                  "[ \t]+")         ; itcl m definition
                          "\\([-A-Za-z0-9_:+*]+\\)" ; method name
                          ) 1)
          ("DefMethods" ,(concat "^\\s-*"
                                 (regexp-opt `("constructor"
                                               "destructor") t) ; method name
                                 "[ \t]+{") 1)
          ("ImpMethods" ,(concat
                          (concat "^\\s-*" (regexp-opt `("::oo::define"
                                                         "define"
                                                         "oo::define"
                                                         "::oo::objdefine"
                                                         "objdefine"
                                                         "oo::objdefine"))
                                  "[ \t]+[^ \t]+[ \t]+method[ \t]+") ; tclOO m imp
                          "\\([-A-Za-z0-9_:+*]+\\)"                  ; method name
                          ) 1)
          ("ImpMethods" ,(concat
                          (concat "^\\s-*" (regexp-opt `("body"
                                                         "::itcl::body"
                                                         "itcl::body"
                                                         "configbody"
                                                         "::itcl::configbody"
                                                         "itcl::configbody"))
                                  "[ \t]+")         ; itcl m imp
                          "\\([-A-Za-z0-9_:+*]+\\)" ; itcl method name
                          ) 1)
          ("StaticMethods" ,(concat
                             (concat "^\\s-*" (regexp-opt `("::oo::define"
                                                            "define"
                                                            "oo::define"))
                                     "[ \t]+[^ \t]+[ \t]+self[ \t]+method[ \t]+")
                             "\\([-A-Za-z0-9_:+*]+\\)" ; oo static method name
                             ) 1)
          ("Classes" ,(concat (concat "^\\s-*" (regexp-opt `("class"
                                                             "::oo::class create"
                                                             "oo::class"
                                                             "::itcl::class"
                                                             "itcl::class"))
                                      "[ \t]+")
                              "\\([-A-Za-z0-9_:+*]+\\)" ; class name
                              ) 1))))

(provide 'init-tcl-hm-mode)
;;; init-tcl-hm-mode.el ends here
