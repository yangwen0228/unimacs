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
  ;; (setq tcl-application "c:/Program Files/Altair/14.0/hw/bin/win64/hw.exe")
  ;; (setq tcl-command-switches '("/clientconfig" "hwfepre.dat" "-tcl" "D:\\svn\\CRRC_2016\\source codes\\main\\weldCheck.tcl"))
  (add-to-list 'company-keywords-alist
               (append '(tcl-mode) tcl-hm-commands-list))

  (unimacs-company-define-backends
   '((tcl-hm-mode tcl-mode) .
     ((company-keywords company-dabbrev-code)
      company-files company-dabbrev)))

  (use-package company-gtags
    :ensure nil
    :bind* ("C-<tab>" . company-gtags-tcl-rigid)
    :commands (company-gtags company-gtags-tcl-rigid)
    :preface
    (defun company-gtags--fetch-tcl-tags-rigid (prefix)
      ;; (print prefix)
      (with-temp-buffer
        (let (tags)
          ;; -xgq will search both "proc test ..." and "[test ...]"
          ;; -xq will search only "proc test ...", is much faster.
          (when (= 0 (call-process company-gtags-executable nil
                                   (list (current-buffer) nil) nil "-xq"
                                   (concat prefix ".*")))
            ;; (print (buffer-string))
            (goto-char (point-min))
            (cl-loop
             while
             (re-search-forward (concat
                                 "^\\(::\\)?"             ;1 (::)?
                                 "\\([a-zA-Z0-9:._-]*\\)" ;2 completion text
                                 "[ \t]+\\([[:digit:]]+\\)" ;3 line number
                                 "[ \t]+\\([^ \t]+\\)"      ;4 file
                                 "[ \t]*\\(proc[ \t]+\\|.*?\\[\\)\\(::\\)?\\([a-zA-Z0-9:._-]+::\\)*?" ; 5 6 7 filter
                                 "\\(.*\\)" ;8 definition
                                 "$"
                                 ) nil t)
             collect
             (propertize (concat (match-string 1) (match-string 2))
                         'meta (concat (match-string 5) (match-string 6) (match-string 7) (match-string 8) "\n=> file: " (match-string 4))
                         'location (cons (expand-file-name (match-string 4))
                                         (string-to-number (match-string 3)))))))))

    (defun company-grab-symbol-for-tcl-rigid ()
      "If point is at the end of a symbol, return it.
Otherwise, if point is not inside a symbol, return an empty string."
      (if (or (looking-at "\\_>")
              (looking-at "$"))
          (buffer-substring-no-properties
           (point)
           (save-excursion (skip-syntax-backward "w_.") (point)))
        (unless (and (char-after)
                     (memq (char-syntax (char-after)) '(?w ?_)))
          "")))

    (defun company-gtags-tcl-rigid (command &optional arg &rest ignored)
      "Support for tcl ns::proc kind of command.  COMMAND ARG IGNORED."
      (interactive (list 'interactive))
      (cl-case command
        (interactive (company-begin-backend 'company-gtags-tcl-rigid))
        (prefix (and company-gtags-executable
                     buffer-file-name
                     (apply #'derived-mode-p company-gtags-modes)
                     (not (company-in-string-or-comment))
                     (company-gtags--tags-available-p)
                     (or (company-grab-symbol-for-tcl-rigid) 'stop)))
        (candidates (company-gtags--fetch-tcl-tags-rigid arg))
        (sorted t)
        (duplicates t)
        (annotation (company-gtags--annotation arg))
        (meta (get-text-property 0 'meta arg))
        (location (get-text-property 0 'location arg))
        (post-completion (let ((anno (company-gtags--annotation arg)))
                           (when (and company-gtags-insert-arguments anno)
                             (insert anno)
                             (company-template-c-like-templatify anno))))))
    )

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

  (defun tcl-hm-copy-path-source-this-file ()
    (interactive)
    (let ((filename (if (equal major-mode 'dired-mode)
                        default-directory
                      (buffer-file-name))))
      (when filename
        (kill-new (concat "source {" filename "}"))
        (message "Copied command 'source {%s}' to the clipboard." filename))))

  (defun tcl-eval-buffer (&optional and-go)
    "Send the whole buffer to the inferior Tcl process.
Prefix argument means switch to the Tcl buffer afterwards."
    (interactive "P")
    (save-excursion
      (end-of-buffer)
      (let ((end (point)))
        (beginning-of-buffer)
        (tcl-eval-region (point) end)))
    (if and-go (switch-to-tcl t)))

  (define-key tcl-hm-mode-map "{"        nil)
  (define-key tcl-hm-mode-map "}"        nil)
  (define-key tcl-hm-mode-map "["        nil)
  (define-key tcl-hm-mode-map "]"        nil)
  (define-key tcl-hm-mode-map ";"        'tcl-electric-char)
  (define-key tcl-hm-mode-map "#"        'tcl-electric-hash) ;Remove?  -stef
  (define-key tcl-hm-mode-map "\t"       'tcl-indent-command)
  (define-key tcl-hm-mode-map "\M-\C-x"  'tcl-eval-defun)
  (define-key tcl-hm-mode-map "\C-c\C-j" 'tcl-help-on-hm-word)
  (define-key global-map "\C-c\C-j" 'tcl-help-on-hm-word)
  (define-key tcl-hm-mode-map "\C-c\C-v" 'tcl-eval-defun)
  (define-key tcl-hm-mode-map "\C-c\C-b" 'tcl-eval-buffer)
  (define-key tcl-mode-map "\C-c\C-b" 'tcl-eval-buffer)
  (define-key tcl-hm-mode-map "\C-c\C-f" 'tcl-load-file)
  (define-key tcl-hm-mode-map "\C-c\C-t" 'inferior-tcl)
  (define-key tcl-hm-mode-map "\C-c\C-x" 'tcl-eval-region)

  (add-to-list 'tcl-help-directory-list
               "C/Program Files (x86)/TclPro1.4/lib/tclX8.3/help/tcl/")
  (add-to-list 'tcl-help-directory-list
               "C:/Program Files (x86)/TclPro1.4/lib/tkX8.3/help/tk/")

  (defun tcl-hm-set-help-version (version)
    "Change HyperWorks help VERSION."
    (interactive (list (read-version)))
    (setq tcl-hm-help-directory-list nil)
    (add-to-list 'tcl-hm-help-directory-list
                 (concat "C:/Program Files/Altair/" version "/hw/tcl/hwt/docs"))
    (add-to-list 'tcl-hm-help-directory-list
                 (concat "C:/Program Files/Altair/" version "/help/hwdref/"))
    )
  (defsubst read-version ()
    (let ((versions '("12.0" "13.0" "14.0")))
      (completing-read "HyperWorks Version(Default: 13.0): " versions nil t nil nil "13.0")))
  (tcl-hm-set-help-version "12.0")

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
                     "\\([^ \t]+\\)"                         ; proc name
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
