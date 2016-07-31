;;; init-org.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (add-hook 'org-mode-hook 'org-indent-mode)
  (use-package cnblogs
    :ensure nil
    :init
    (require 'cnblogs)
    (cnblogs-minor-mode t)
    (setq org2blog/wp-blog-alist
          '(("cnblogs"
             :url "http://www.cnblogs.com/yangwen0228/services/metaWeblog.aspx"
             :username "yangwen0228"
             :default-categories ("emacs")
             :keep-new-lines t
             :confirm t
             :wp-code nil
             :tags-as-categories nil)))
    (bind-keys ("C-c c p" . cnblogs-new-post)
               ("C-c c e" . cnblogs-edit-post)
               ("C-c c d" . cnblogs-delete-post))
    )

  (defun org-make-code-block ()
    (interactive)
    (let ((beg (if (region-active-p) (region-beginning) (point)))
          (end (if (region-active-p) (region-end) (point))))
      (goto-char beg)
      (evil-open-above 1)
      (insert "#+BEGIN_SRC ")
      (goto-char (+ end 12))
      (evil-open-below 0)
      (insert "#+END_SRC")
      (goto-char (+ beg 12))
      ))

  ;; {{ export org-mode in Chinese into PDF
  ;; @see http://freizl.github.io/posts/tech/2012-04-06-export-orgmode-file-in-Chinese.html
  ;; and you need install texlive-xetex on different platforms
  ;; To install texlive-xetex:
  ;;    `sudo USE="cjk" emerge texlive-xetex` on Gentoo Linux
  (setq org-latex-to-pdf-process ;; org v7
        '("xelatex -interaction nonstopmode -output-directory %o %f"
          "xelatex -interaction nonstopmode -output-directory %o %f"
          "xelatex -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-pdf-process org-latex-to-pdf-process) ;; org v8
  ;; }}

  ;; Various preferences
  (setq org-log-done t
        org-completion-use-ido t
        org-edit-src-content-indentation 0
        org-edit-timestamp-down-means-later t
        org-agenda-start-on-weekday nil
        org-agenda-span 14
        org-agenda-include-diary t
        org-agenda-window-setup 'current-window
        org-fast-tag-selection-single-key 'expert
        org-export-kill-product-buffer-when-displayed t
        ;; org v7
        org-export-odt-preferred-output-format "doc"
        ;; org v8
        org-odt-preferred-output-format "doc"
        org-tags-column 80
        ;;org-startup-indented t
        )

  ;; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
  ;; Targets start with the file name - allows creating level 1 tasks
  (setq org-refile-use-outline-path (quote file))
  ;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
  (setq org-outline-path-complete-in-steps t)

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                (sequence "WAITING(w@/!)" "SOMEDAY(S)" "PROJECT(P@)" "|" "CANCELLED(c@/!)"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Change task state to STARTED when clocking in
  (setq org-clock-in-switch-to-state "STARTED")
  ;; Save clock data and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
  ;; Removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)

  ;; Show the clocked-in task - if any - in the header line
  (defun sanityinc/show-org-clock-in-header-line ()
    (setq-default header-line-format '((" " org-mode-line-string " "))))

  (defun sanityinc/hide-org-clock-from-header-line ()
    (setq-default header-line-format nil))

  (add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
  (add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
  (add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

  (eval-after-load 'org-clock
    '(progn
       (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
       (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu)))

  (eval-after-load 'org
    '(progn
       (require 'org-clock)
                                        ; @see http://irreal.org/blog/?p=671
       (setq org-src-fontify-natively t)
       ;;(require 'org-checklist)
       (require 'org-fstree)
       (setq org-ditaa-jar-path (format "%s%s" (if *cygwin* "c:/cygwin" "")
                                        (expand-file-name "contrib/scripts/ditaa.jar" unimacs-elpa-dir)) )
       (define-key org-mode-map "\C-cb" 'org-make-code-block)

       (defun soft-wrap-lines ()
         "Make lines wrap at window edge and on word boundary,
        in current buffer."
         (interactive)
         (setq truncate-lines nil)
         (setq word-wrap t)
         )
       (add-hook 'org-mode-hook '(lambda ()
                                   (setq evil-auto-indent nil)
                                   (soft-wrap-lines)
                                   ))))

  (defadvice org-open-at-point (around org-open-at-point-choose-browser activate)
    (let ((browse-url-browser-function
           (cond ((equal (ad-get-arg 0) '(4))
                  'browse-url-generic)
                 ((equal (ad-get-arg 0) '(16))
                  'choose-browser)
                 (t
                  (lambda (url &optional new)
                    (w3m-browse-url url t))))))
      ad-do-it))

  ;; {{ org2nikola set up
  (setq org2nikola-output-root-directory (expand-file-name "projs/blog.yangwen0228.org" unimacs-personal-dir))
  (setq org2nikola-use-google-code-prettify t)
  (setq org2nikola-prettify-unsupported-language
        '(elisp "lisp"
                emacs-lisp "lisp"))
  ;; }}

  ;; @see http://stackoverflow.com/questions/6014181/org-mode-exporting-to-pdf-for-emacs-app-on-mac-os-x
  ;; Export org docments to pdf files.
  ;; (require 'org-docbook)
  ;;
  (setq org-export-docbook-xsl-fo-proc-command "/usr/local/bin/fop \"%i\" \"%o\"")
  (setq org-export-docbook-xslt-proc-command "/usr/local/bin/saxon -o:\"%o\" -s:\"%i\" -xsl:\"%s\"")
  (setq org-export-docbook-xslt-stylesheet "/usr/local/Cellar/docbook-xsl/1.78.1/docbook-xsl-ns/fo/docbook.xsl")

  ;; @see http://emacs-fu.blogspot.com/2011/04/nice-looking-pdfs-with-org-mode-and.html
  ;; 'cn-org-article' for export org documents to the LaTex 'article', using
  ;; XeTeX and some fancy fonts; requires XeTeX (see org-latex-to-pdf-process)

  ;; This may need in the future:
  ;; \\setromanfont{Gentium}
  ;; \\setromanfont [BoldFont={Gentium Basic Bold},
  ;;                 ItalicFont={Gentium Basic Italic}]{Gentium Basic}
  ;; \\setsansfont{Charis SIL}
  ;; \\setmonofont[Scale=0.8]{DejaVu Sans Mono}
  ;; (require 'org-latex)
  ;; (add-to-list 'org-export-latex-classes
  ;;   '("cn-org-article"
  ;; "\\documentclass[11pt,a4paper]{article}
  ;; \\usepackage[T1]{fontenc}
  ;; \\usepackage{fontspec}
  ;; \\usepackage{graphicx}
  ;; \\defaultfontfeatures{Mapping=tex-text}
  ;; \\setmainfont{SimSun}
  ;; \\usepackage{geometry}
  ;; \\geometry{a4paper, textwidth=6.5in, textheight=10in,
  ;;             marginparsep=7pt, marginparwidth=.6in}
  ;; \\pagestyle{empty}
  ;; \\title{}
  ;;       [NO-DEFAULT-PACKAGES]
  ;;       [NO-PACKAGES]"
  ;;      ("\\section{%s}" . "\\section*{%s}")
  ;;      ("\\subsection{%s}" . "\\subsection*{%s}")
  ;;      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ;;      ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ;;      ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  )

(provide 'init-org)
;;; init-org.el ends here
