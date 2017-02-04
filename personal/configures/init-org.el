;;; init-org.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind ("C-c a" . org-agenda)
  :config
  (add-hook 'org-mode-hook 'org-indent-mode)
  (use-package cnblogs
    :ensure nil
    :init
    (require 'cnblogs)
    (cnblogs-minor-mode t)
    ;; Run command: cnblogs-setup-blog to set up, blog-id == username.
    (bind-keys ("C-c c p" . cnblogs-post)
               ("C-c c n" . cnblogs-new-post)
               ("C-c c e" . cnblogs-edit-post)
               ("C-c c d" . cnblogs-delete-post))
    ;; Bugfix: error url-http-create-request: Multibyte text in HTTP request
    ;; @ http://www.cnblogs.com/yangwen0228/p/6238528.html
    (defun url-http-create-request (&optional ref-url)
      "Create an HTTP request for `url-http-target-url', referred to by REF-URL."
      (let* ((extra-headers)
             (request nil)
             (no-cache (cdr-safe (assoc "Pragma" url-http-extra-headers)))
             (using-proxy url-http-proxy)
             (proxy-auth (if (or (cdr-safe (assoc "Proxy-Authorization"
                                                  url-http-extra-headers))
                                 (not using-proxy))
                             nil
                           (let ((url-basic-auth-storage
                                  'url-http-proxy-basic-auth-storage))
                             (url-get-authentication url-http-proxy nil 'any nil))))
             (real-fname (url-filename url-http-target-url))
             (host (url-http--encode-string (url-host url-http-target-url)))
             (auth (if (cdr-safe (assoc "Authorization" url-http-extra-headers))
                       nil
                     (url-get-authentication (or
                                              (and (boundp 'proxy-info)
                                                   proxy-info)
                                              url-http-target-url) nil 'any nil))))
        (if (equal "" real-fname)
            (setq real-fname "/"))
        (setq no-cache (and no-cache (string-match "no-cache" no-cache)))
        (if auth
            (setq auth (concat "Authorization: " auth "\r\n")))
        (if proxy-auth
            (setq proxy-auth (concat "Proxy-Authorization: " proxy-auth "\r\n")))

        ;; Protection against stupid values in the referrer
        (if (and ref-url (stringp ref-url) (or (string= ref-url "file:nil")
                                               (string= ref-url "")))
            (setq ref-url nil))

        ;; We do not want to expose the referrer if the user is paranoid.
        (if (or (memq url-privacy-level '(low high paranoid))
                (and (listp url-privacy-level)
                     (memq 'lastloc url-privacy-level)))
            (setq ref-url nil))

        ;; url-http-extra-headers contains an assoc-list of
        ;; header/value pairs that we need to put into the request.
        (setq extra-headers (mapconcat
                             (lambda (x)
                               (concat (car x) ": " (cdr x)))
                             url-http-extra-headers "\r\n"))
        (if (not (equal extra-headers ""))
            (setq extra-headers (concat extra-headers "\r\n")))

        ;; This was done with a call to `format'.  Concatenating parts has
        ;; the advantage of keeping the parts of each header together and
        ;; allows us to elide null lines directly, at the cost of making
        ;; the layout less clear.
        (setq request
              (concat
               ;; The request
               (or url-http-method "GET") " "
               (url-http--encode-string
                (if using-proxy (url-recreate-url url-http-target-url) real-fname))
               " HTTP/" url-http-version "\r\n"
               ;; Version of MIME we speak
               "MIME-Version: 1.0\r\n"
               ;; (maybe) Try to keep the connection open
               "Connection: " (if (or using-proxy
                                      (not url-http-attempt-keepalives))
                                  "close" "keep-alive") "\r\n"
                                  ;; HTTP extensions we support
                                  (if url-extensions-header
                                      (format
                                       "Extension: %s\r\n" url-extensions-header))
                                  ;; Who we want to talk to
                                  (if (/= (url-port url-http-target-url)
                                          (url-scheme-get-property
                                           (url-type url-http-target-url) 'default-port))
                                      (format
                                       "Host: %s:%d\r\n" (puny-encode-domain host)
                                       (url-port url-http-target-url))
                                    (format "Host: %s\r\n" (puny-encode-domain host)))
                                  ;; Who its from
                                  (if url-personal-mail-address
                                      (concat
                                       "From: " url-personal-mail-address "\r\n"))
                                  ;; Encodings we understand
                                  (if (or url-mime-encoding-string
                                          ;; MS-Windows loads zlib dynamically, so recheck
                                          ;; in case they made it available since
                                          ;; initialization in url-vars.el.
                                          (and (eq 'system-type 'windows-nt)
                                               (fboundp 'zlib-available-p)
                                               (zlib-available-p)
                                               (setq url-mime-encoding-string "gzip")))
                                      (concat
                                       "Accept-encoding: " url-mime-encoding-string "\r\n"))
                                  (if url-mime-charset-string
                                      (concat
                                       "Accept-charset: "
                                       (url-http--encode-string url-mime-charset-string)
                                       "\r\n"))
                                  ;; Languages we understand
                                  (if url-mime-language-string
                                      (concat
                                       "Accept-language: " url-mime-language-string "\r\n"))
                                  ;; Types we understand
                                  "Accept: " (or url-mime-accept-string "*/*") "\r\n"
                                  ;; User agent
                                  (url-http-user-agent-string)
                                  ;; Proxy Authorization
                                  proxy-auth
                                  ;; Authorization
                                  auth
                                  ;; Cookies
                                  (when (url-use-cookies url-http-target-url)
                                    (url-http--encode-string
                                     (url-cookie-generate-header-lines
                                      host real-fname
                                      (equal "https" (url-type url-http-target-url)))))
                                  ;; If-modified-since
                                  (if (and (not no-cache)
                                           (member url-http-method '("GET" nil)))
                                      (let ((tm (url-is-cached url-http-target-url)))
                                        (if tm
                                            (concat "If-modified-since: "
                                                    (url-get-normalized-date tm) "\r\n"))))
                                  ;; Whence we came
                                  (if ref-url (concat
                                               "Referer: " ref-url "\r\n"))
                                  extra-headers
                                  ;; Length of data
                                  (if url-http-data
                                      (concat
                                       "Content-length: " (number-to-string
                                                           (length url-http-data))
                                       "\r\n"))
                                  ;; End request
                                  "\r\n"
                                  ;; Any data
                                  url-http-data))
        ;; Bug#23750
        (setq request (url-http--encode-string request))
        (unless (= (string-bytes request)
                   (length request))
          (error "Multibyte text in HTTP request: %s" request))
        (url-http-debug "Request is: \n%s" request)
        request))
    )

  (use-package org-download
    :bind ("C-S-y" . org-download-clipboard)
    :config
    (defun org-download-clipboard ()
      "Save the captured image from clipboard to file, and insert into buffer. Or org-download-yank."
      (interactive)
      (let ((link "d:/temp/screenshot.png"))
        (if (eq 0 (shell-command (format "\"c:/Program\sFiles/ImageMagick-6.9.3-Q16/convert.exe\" clipboard:myimage %s" link) "*screenshot2file*" "*screenshot2file*"))
            (org-download-image link)
          (org-download-yank)))))

  ;; Various preferences
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

  (defun my/org-inline-css-hook (exporter)
    "Insert custom inline css to automatically set the
background of code to whatever theme I'm using's background"
    (when (eq exporter 'html)
      (let* ((my-pre-bg (face-background 'default))
             (my-pre-fg (face-foreground 'default)))
        (setq
         org-html-head-extra
         (concat
          org-html-head-extra
          (format "<style type=\"text/css\">\n code {color: #FF0000}\n pre.src {background-color: %s; color: %s;}</style>\n"
                  my-pre-bg my-pre-fg))))))

  (add-hook 'org-export-before-processing-hook 'my/org-inline-css-hook)

  (setq org-emphasis-alist '(("*" bold)
                             ("/" italic)
                             ("_" underline)
                             ("=" org-verbatim verbatim)
                             ("~" (:foreground "red"))
                             ("+"
                              (:strike-through t))))

  (setq org-log-done t
        org-completion-use-ido t
        org-edit-src-content-indentation 0
        org-edit-timestamp-down-means-later t
        org-fast-tag-selection-single-key 'expert
        org-export-kill-product-buffer-when-displayed t
        ;; org v7
        org-export-odt-preferred-output-format "doc"
        ;; org v8
        org-odt-preferred-output-format "doc"
        org-tags-column 80
        ;;org-startup-indented t
        org-export-with-creator t
        org-src-fontify-natively t
        )

  (use-package org-agenda
    :ensure nil
    :preface
    (defun org-update-agenda-files ()
      "Update the agenda files under a directory!"
      (interactive)
      (let ((my-agenda-directory "d:/orgs/notes")
            org-files)
        (dolist (org-file (f-files my-agenda-directory))
          (when (string= (f-ext org-file) "org")
            (setq org-files (append org-files (list org-file)))))
        (setq org-agenda-files org-files)))

    :config
    (defadvice org-agenda (before org-agenda activate)
      (org-update-agenda-files))

    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                  (sequence "WAITING(w@/!)" "SOMEDAY(S)" "PROJECT(P@)" "|" "CANCELLED(c@/!)"))))
    (setq org-agenda-start-on-weekday nil
          org-agenda-span 14
          org-agenda-include-diary t
          org-agenda-window-setup 'current-window
          ))

  (use-package org-clock
    :ensure nil
    :config
    ;; Change task state to STARTED when clocking in
    (setq org-clock-in-switch-to-state "STARTED")
    ;; Save clock data and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    ;; Removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t)

    (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
    (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu)
    ;; Show the clocked-in task - if any - in the header line
    (defun org-clock-show-in-header-line ()
      (setq-default header-line-format '((" " org-mode-line-string " "))))

    (defun org-clock-hide-from-header-line ()
      (setq-default header-line-format nil))

    (add-hook 'org-clock-in-hook 'org-clock-show-in-header-line)
    (add-hook 'org-clock-out-hook 'org-clock-hide-from-header-line)
    (add-hook 'org-clock-cancel-hook 'org-clock-hide-from-header-line))

  (use-package org-fstree :disabled)

  (use-package org-capture
    :disabled
    :ensure nil
    :config
    (setq org-default-notes-file (expand-file-name "notes.org" org-directory))
    (setq org-capture-templates
          '(("t" "Todo" entry         (file+headline  "~/org/agenda.org" "Tasks")        "** TODO %?\n  %i\n  %a"  :prepend t)
            ("n" "Notes" entry        (file+headline  "~/org/notes.org"  "General")      "* %T %?\n\n  %i\n"       :prepend t)
            ))
    (global-set-key (kbd "C-c k") 'org-capture))

  (use-package org-latex
    :disabled
    :ensure nil
    :config
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
    )

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
