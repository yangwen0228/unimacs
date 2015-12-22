(require 'tcl-hm-mode)
(autoload 'tcl-hm-mode "tcl-hm-mode" "Tcl hm Mode" t)
(add-to-list 'auto-mode-alist '("\\.tcl\\'" . tcl-hm-mode))
(add-to-list 'auto-mode-alist '("\\.test\\'" . tcl-hm-mode))
(add-to-list 'auto-mode-alist '("\\.wgt\\'" . tcl-hm-mode))
(add-to-list 'auto-mode-alist '("\\.msg\\'" . tcl-hm-mode))

(setq tcl-imenu-generic-expression
      `(
        ("Variables" ,(concat "^\\s-*variable[ \t]+" "\\([-A-Za-z0-9_:+*]+\\)" "[ \t]+[^ \t]+") 1)
        ("Procs" ,(concat (concat "^\\s-*" (regexp-opt `("proc")) "[ \t]+") "\\([-A-Za-z0-9_:+*]+\\)") 1)
        ("ClassProcs" ,(concat (concat "^\\s-*" (regexp-opt `("protected proc" "private proc" "public proc")) "[ \t]+") "\\([-A-Za-z0-9_:+*]+\\)") 1)
        ("DefMethods" ,(concat (concat "^\\s-*" (regexp-opt `("method" "public method" "protected method" "private method")) "[ \t]+") "\\([-A-Za-z0-9_:+*]+\\)") 1)
        ("DefMethods" ,(concat (concat "^\\s-*" (regexp-opt `("constructor" "destructor") t) "[ \t]+") "") 1)
        ("ImpMethods" ,(concat (concat "^\\s-*" (regexp-opt `("::oo::define" "define" "oo::define" "::oo::objdefine" "objdefine" "oo::objdefine")) "[ \t]+[^ \t]+[ \t]+method[ \t]+") "\\([-A-Za-z0-9_:+*]+\\)") 1)
        ("ImpMethods" ,(concat (concat "^\\s-*" (regexp-opt `("body" "::itcl::body" "itcl::body" "configbody" "::itcl::configbody" "itcl::configbody")) "[ \t]+") "\\([-A-Za-z0-9_:+*]+\\)") 1)
        ("StaticMethods" ,(concat (concat "^\\s-*" (regexp-opt `("::oo::define" "define" "oo::define")) "[ \t]+[^ \t]+[ \t]+self[ \t]+method[ \t]+") "\\([-A-Za-z0-9_:+*]+\\)") 1)
        ("Classes" ,(concat (concat "^\\s-*" (regexp-opt `("class" "::oo::class create" "oo::class" "::itcl::class" "itcl::class")) "[ \t]+") "\\([-A-Za-z0-9_:+*]+\\)") 1)
        ))

;; In the last line the pattern for matching a file name or file extension can
;; be changed to any naming convention for hm script files.  Also, the last
;; line can be omitted and Tcl hm Mode be automatically invoked only for files
;; that have the following pattern on the first line:

;;   # -*- tcl-hm -*-

;; Alternatively, the following line can be used to always load Tcl hm Mode
;; along with Tcl mode:

;;   (add-hook 'tcl-mode-hook '(lambda () (require 'tcl-hm-mode) (tcl-hm-mode)))
;;
(modify-syntax-entry ?* "w" tcl-mode-syntax-table)
(modify-syntax-entry ?_ "w" tcl-mode-syntax-table)
(modify-syntax-entry ?, "_" tcl-mode-syntax-table)

(modify-syntax-entry ?! "." tcl-mode-syntax-table)
(modify-syntax-entry ?$ "." tcl-mode-syntax-table)
(modify-syntax-entry ?. "." tcl-mode-syntax-table)
(modify-syntax-entry ?+ "." tcl-mode-syntax-table)
(modify-syntax-entry ?- "." tcl-mode-syntax-table)
(modify-syntax-entry ?/ "." tcl-mode-syntax-table)
(modify-syntax-entry ?: "." tcl-mode-syntax-table)
(modify-syntax-entry ?= "_" tcl-mode-syntax-table)

(add-to-list 'tcl-help-directory-list "/Volumes/C/Program Files (x86)/TclPro1.4/lib/tclX8.3/help/tcl/")
(add-to-list 'tcl-help-directory-list "/Volumes/C/Program Files (x86)/TclPro1.4/lib/tkX8.3/help/tk/")

(add-to-list 'tcl-hm-help-directory-list "C:Program Files/Altair/12.0/hw/tcl/hwt/docs")
(add-to-list 'tcl-hm-help-directory-list "C:Program Files/Altair/12.0/help/hwdref/")

(define-key tcl-hm-mode-map "{"        'tcl-electric-char)
(define-key tcl-hm-mode-map "}"        'tcl-electric-brace)
(define-key tcl-hm-mode-map "["        'tcl-electric-char)
(define-key tcl-hm-mode-map "]"        'tcl-electric-char)
(define-key tcl-hm-mode-map ";"        'tcl-electric-char)
(define-key tcl-hm-mode-map "#"        'tcl-electric-hash) ;Remove?  -stef
(define-key tcl-hm-mode-map "\e\C-q"   'tcl-indent-exp)
(define-key tcl-hm-mode-map "\177"     'backward-delete-char-untabify)
(define-key tcl-hm-mode-map "\t"       'tcl-indent-command)
(define-key tcl-hm-mode-map "\M-\C-x"  'tcl-eval-defun)
;; (define-key tcl-hm-mode-map "\C-c\C-j" 'tcl-help-on-hm-word)
;; (define-key tcl-hm-mode-map "\C-c\C-k" 'tcl-help-on-word)
(define-key tcl-hm-mode-map "\C-c\C-v" 'tcl-eval-defun)
(define-key tcl-hm-mode-map "\C-c\C-f" 'tcl-load-file)
(define-key tcl-hm-mode-map "\C-c\C-t" 'inferior-tcl)
(define-key tcl-hm-mode-map "\C-c\C-x" 'tcl-eval-region)

;; change the key binding to the global mode, so that we can check the keyword from the help document
(global-set-key "\C-c\C-j" 'tcl-help-on-hm-word)
(global-set-key "\C-c\C-k" 'tcl-help-on-word)

(require 'tcl-hm-eldoc)
(add-hook 'tcl-mode-hook 'tcl-hm-eldoc)

(provide 'init-tcl-hm-mode)
