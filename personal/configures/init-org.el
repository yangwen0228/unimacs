;;; init-org.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c a" . org-agenda)
         ("C-c k" . org-capture))
  :config
  (add-hook 'org-mode-hook 'org-indent-mode)
  (unbind-key "C-a" org-mode-map)
  (unbind-key "C-e" org-mode-map)
  (use-package htmlize)
  (use-package epresent
    :commands epresent-run
    :config
    (setq epresent-text-scale 16)
    (define-derived-mode epresent-mode org-mode "EPresent"
      "Lalala."
      ;; make Org-mode be as pretty as possible
      (add-hook 'org-src-mode-hook 'epresent-setup-src-edit)
      (setq epresent-inline-image-overlays org-inline-image-overlays)
      (setq epresent-src-fontify-natively org-src-fontify-natively)
      (setq org-src-fontify-natively t)
      (setq org-fontify-quote-and-verse-blocks t)
      (setq epresent-hide-emphasis-markers org-hide-emphasis-markers)
      (setq org-hide-emphasis-markers t)
      (setq epresent-outline-ellipsis
            (display-table-slot standard-display-table 'selective-display))
      (set-display-table-slot standard-display-table 'selective-display [32])
      (setq epresent-pretty-entities org-pretty-entities)
      (setq org-hide-pretty-entities t)
      (setq mode-line-format (epresent-get-mode-line))
      (add-hook 'org-babel-after-execute-hook 'epresent-refresh)
      (condition-case ex
          (let ((org-format-latex-options
                 (plist-put (copy-tree org-format-latex-options)
                            :scale epresent-format-latex-scale)))
            (org-preview-latex-fragment '(16)))
        ('error
         (message "Unable to imagify latex [%s]" ex)))
      (unimacs-set-font epresent--frame epresent-text-scale)
      ;; (set-face-attribute 'default epresent--frame :height epresent-text-scale)
      ;; fontify the buffer
      (add-to-invisibility-spec '(epresent-hide))
      ;; remove flyspell overlays
      (flyspell-mode-off)
      (epresent-fontify))
    )
  (use-package cnblogs :ensure nil
    :init
    (use-package xml-rpc :init (require 'xml-rpc))
    (setq cnblogs-file-root-path (expand-file-name "cnblogs/" unimacs-tempfiles-dir))
    (require 'cnblogs)
    (cnblogs-minor-mode t)
    ;; Run command: cnblogs-setup-blog to set up, blog-id == username.
    (bind-keys ("C-c c p" . cnblogs-post)
               ("C-c c n" . cnblogs-new-post)
               ("C-c c e" . cnblogs-edit-post)
               ("C-c c d" . cnblogs-delete-post))
    ;; Bugfix: error url-http-create-request: Multibyte text in HTTP request
    ;; @ http://www.cnblogs.com/yangwen0228/p/6238528.html
    (when (fboundp 'url-http--encode-string)
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
                                         "Host: %s:%d\r\n" host (url-port url-http-target-url))
                                      (format "Host: %s\r\n" host))
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
      ))

  (use-package org-download
    :bind ("C-S-y" . org-download-clipboard)
    :init
    (require 'org-download)
    (cond
     (*win32*
      (setq org-download-clipboard-temp-file "d:/temp/screenshot.png")
      (setq org-download-clipboard-cmd (format "\"c:/Program\sFiles/ImageMagick-6.9.3-Q16/convert.exe\" clipboard:myimage %s" org-download-clipboard-temp-file)))
     (*is-a-mac*
      ;; brew install pngpaste
      (setq org-download-clipboard-temp-file "/tmp/screenshot.png")
      (setq org-download-clipboard-cmd (format "pngpaste %s && convert %s -scale 50%% %s" org-download-clipboard-temp-file org-download-clipboard-temp-file org-download-clipboard-temp-file))))
    (defun org-download-clipboard ()
      "Save the captured image from clipboard to file, and insert into buffer. Or org-download-yank."
      (interactive)
      (if (eq 0 (shell-command org-download-clipboard-cmd "*screenshot2file*" "*screenshot2file*"))
          (org-download-image org-download-clipboard-temp-file)
        (org-download-yank))))

  ;; browse links
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
         (format "<style type=\"text/css\">\n code {color: #FF0000}\n pre.src {background-color: %s; color: %s;}</style>\n"
                 my-pre-bg my-pre-fg)))))

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
        org-odt-preferred-output-format "doc"
        org-tags-column 80
        org-startup-indented t
        org-src-fontify-natively t
        )

  ;; org-export settings:
  (setq org-export-with-creator t
        org-export-preserve-breaks t    ; return as new line
        org-export-babel-evaluate nil   ; don't evaluate codeblock during export.
        org-export-with-sub-superscripts nil
        org-export-kill-product-buffer-when-displayed t
        )
  (require 'ox-freemind nil t)

  (use-package org-agenda :ensure nil
    :init
    (defun org-update-agenda-files ()
      "Update the agenda files under a directory!"
      (interactive)
      (let ((my-agenda-directory org-directory)
            org-files)
        (dolist (org-file (f-files my-agenda-directory))
          (when (and (string= (f-ext org-file) "org")
                     (not (string= (substring (f-filename org-file) 0 2) ".#")))
            (setq org-files (append org-files (list org-file)))))
        (setq org-agenda-files org-files)))

    (defadvice org-agenda (before org-agenda activate)
      (org-update-agenda-files))

    (setq org-columns-default-format "%TODO %50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
    (setq org-agenda-start-on-weekday nil
          org-agenda-span 14
          org-agenda-include-diary t
          org-agenda-window-setup 'current-window
          ))

  (use-package org-alert
    :commands (org-alert-enable org-alert-disable)
    :config
    (setq org-alert-interval 300)
    (setq alert-default-style 'message))

  (use-package org-clock :ensure nil
    :init
    (require 'org-clock)
    ;; Insinuate it everywhere
    (org-clock-persistence-insinuate)
    ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
    (setq org-clock-history-length 23
          ;; Change task state to STARTED when clocking in
          org-clock-in-switch-to-state "STARTED"
          ;; Change task state to DONE when clocking out
          org-clock-out-switch-to-state "HOLD"
          ;; Resume clocking task on clock-in if the clock is open
          org-clock-in-resume t
          ;; Separate drawers for clocking and logs
          org-drawers '("PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS" "HIDDEN")
          ;; Save clock data and state changes and notes in the LOGBOOK drawer
          org-clock-into-drawer t
          ;; Sometimes I change tasks I'm clocking quickly -
          ;; this removes clocked tasks with 0:00 duration
          org-clock-out-remove-zero-time-clocks t
          ;; Clock out when moving task to a done state
          org-clock-out-when-done t
          ;; Enable auto clock resolution for finding open clocks
          org-clock-auto-clock-resolution #'when-no-clock-is-running
          ;; some default parameters for the clock report
          org-agenda-clockreport-parameter-plist
          '(:maxlevel 10 :fileskip0 t :score agenda :block thismonth :compact t :narrow 60))
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

  (use-package org-capture :ensure nil
    :init
    (setq org-directory (cond
                         (*win32* "d:/orgs/notes")
                         (*is-a-mac* "~/Documents/orgs/notes")))
    (setq org-default-notes-file (expand-file-name "refile.org" org-directory))
    (setq org-default-diary-file (expand-file-name "diary.org" org-directory))

    ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
    (setq org-capture-templates
          (quote (("d" "Diary"        entry (file+datetree org-default-diary-file)
                   "* %?\n%U\n" :clock-in t :clock-resume t)
                  ("t" "todo"         entry (file org-default-notes-file)
                   "* TODO %?\n%U\n" :clock-in t :clock-resume t)
                  ("b" "bug"          entry (file org-default-notes-file)
                   "* TODO Bug: %? :BUG:\n%U\n" :clock-in t :clock-resume t)
                  ("r" "respond"      entry (file org-default-notes-file)
                   "* STARTED Respond to: %?\nSCHEDULED: %t\n%U\n" :clock-in t :clock-resume t :immediate-finish nil)
                  ("n" "note"         entry (file org-default-notes-file)
                   "* TODO Note: %? :NOTE:\n%U\n" :clock-in t :clock-resume t)
                  ("m" "Meeting"      entry (file org-default-notes-file)
                   "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                  ("p" "Phone call"   entry (file org-default-notes-file)
                   "* PHONE with %? :PHONE:\n%U" :clock-in t :clock-resume t)
                  ("h" "Habit"        entry (file org-default-notes-file)
                   "* STARTED %?\n%U\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: STARTED"))))
    ;; Targets include this file and any file contributing to the agenda - up to 1 levels deep
    (setq org-refile-targets (quote ((nil :maxlevel . 1)
                                     (org-agenda-files :maxlevel . 1))))
    )

  ;; org-goto:
  (setq org-goto-auto-isearch nil)

  (use-package org-time :ensure nil
    :commands org-time-summary
    :config
    (setq org-time-python "python2")
    (defadvice org-time-summary (before org-time-summary activate)
      (org-update-agenda-files)))

  (use-package org-latex :disabled
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

  ;; active Org-babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(;; other Babel languages
     (plantuml . t)
     (dot . t)))
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  (setq org-plantuml-jar-path
        (expand-file-name "jars/plantuml.jar" unimacs-utils-dir))
  (use-package plantuml-mode
    :init
    (setq plantuml-jar-path org-plantuml-jar-path
          plantuml-java-args '("-Djava.awt.headless=true" "-jar")))

  ;; export to MicroOffice Word:
  (require 'ox-odt)
  ;; org v7
  (setq org-export-odt-preferred-output-format "docx")
  ;; org v8
  (setq org-odt-preferred-output-format "docx")
  (let ((cmd (cond
              (*is-a-mac* "/Applications/LibreOffice.app/Contents/MacOS/soffice")
              (*win32* "c:\\Program Files\\LibreOffice 5\\program\\soffice.exe")
              (t "soffice"))))
    (when (file-exists-p cmd)
      ;; org v7
      (setq org-export-odt-convert-processes (list (list "LibreOffice" (concat cmd " --headless --convert-to %f%x --outdir %d %i"))))
      ;; org v8
      (setq org-odt-convert-processes (list (list "LibreOffice" (concat "\"" cmd "\"" " --headless --convert-to %f%x --outdir %d %i"))))))

  ;; Bugfix: If the org file contains Chinese chars, the `org-odt-export-to-odt'
  ;; can't work. Because zip.exe uses the filename as cp936, but the filename is
  ;; utf-8 encoded in fact.
  (defmacro org-odt--export-wrap (out-file &rest body)
    `(let* ((--out-file ,out-file)
            (out-file-type (file-name-extension --out-file))
            (org-odt-xml-files '("META-INF/manifest.xml" "content.xml"
                                 "meta.xml" "styles.xml"))
            ;; Initialize temporary workarea.  All files that end up in
            ;; the exported document get parked/created here.
            (org-odt-zip-dir (file-name-as-directory
                              (make-temp-file (format "%s-" out-file-type) t)))
            (org-odt-manifest-file-entries nil)
            (--cleanup-xml-buffers
             (function
              (lambda nil
                ;; Kill all XML buffers.
                (mapc (lambda (file)
                        (let ((buf (find-buffer-visiting
                                    (concat org-odt-zip-dir file))))
                          (when buf
                            (with-current-buffer buf
                              (set-buffer-modified-p nil)
                              (kill-buffer buf)))))
                      org-odt-xml-files)
                ;; Delete temporary directory and also other embedded
                ;; files that get copied there.
                (delete-directory org-odt-zip-dir t)))))
       (condition-case err
           (progn
             (unless (executable-find "zip")
               ;; Not at all OSes ship with zip by default
               (error "Executable \"zip\" needed for creating OpenDocument files"))
             ;; Do export.  This creates a bunch of xml files ready to be
             ;; saved and zipped.
             (progn ,@body)
             ;; Create a manifest entry for content.xml.
             (org-odt-create-manifest-file-entry "text/xml" "content.xml")
             ;; Write mimetype file
             (let* ((mimetypes
                     '(("odt" . "application/vnd.oasis.opendocument.text")
                       ("odf" .  "application/vnd.oasis.opendocument.formula")))
                    (mimetype (cdr (assoc-string out-file-type mimetypes t))))
               (unless mimetype
                 (error "Unknown OpenDocument backend %S" out-file-type))
               (write-region mimetype nil (concat org-odt-zip-dir "mimetype"))
               (org-odt-create-manifest-file-entry mimetype "/" "1.2"))
             ;; Write out the manifest entries before zipping
             (org-odt-write-manifest-file)
             ;; Save all XML files.
             (mapc (lambda (file)
                     (let ((buf (find-buffer-visiting
                                 (concat org-odt-zip-dir file))))
                       (when buf
                         (with-current-buffer buf
                           ;; Prettify output if needed.
                           (when org-odt-prettify-xml
                             (indent-region (point-min) (point-max)))
                           (save-buffer 0)))))
                   org-odt-xml-files)
             ;; Run zip.
             (let* ((target --out-file)
                    (target-name "temp.odt")
                    (cmds `(("zip" "-mX0" ,target-name "mimetype")
                            ("zip" "-rmTq" ,target-name "."))))
               ;; If a file with same name as the desired output file
               ;; exists, remove it.
               (when (file-exists-p target)
                 (delete-file target))
               ;; Zip up the xml files.
               (let ((coding-system-for-write 'no-conversion) exitcode err-string)
                 (message "Creating ODT file...")
                 ;; Switch temporarily to content.xml.  This way Zip
                 ;; process will inherit `org-odt-zip-dir' as the current
                 ;; directory.
                 (with-current-buffer
                     (find-file-noselect (concat org-odt-zip-dir "content.xml") t)
                   (mapc
                    (lambda (cmd)
                      (message "Running %s" (mapconcat 'identity cmd " "))
                      (setq err-string
                            (with-output-to-string
                              (setq exitcode
                                    (apply 'call-process (car cmd)
                                           nil standard-output nil (cdr cmd)))))
                      (or (zerop exitcode)
                          (error (concat "Unable to create OpenDocument file."
                                         "  Zip failed with error (%s)")
                                 err-string)))
                    cmds)))
               ;; Move the zip file from temporary work directory to
               ;; user-mandated location.
               (rename-file (concat org-odt-zip-dir target-name) target)
               (message "Created %s" (expand-file-name target))
               ;; Cleanup work directory and work files.
               (funcall --cleanup-xml-buffers)
               ;; Open the OpenDocument file in archive-mode for
               ;; examination.
               (find-file-noselect target t)
               ;; Return exported file.
               (cond
                ;; Case 1: Conversion desired on exported file.  Run the
                ;; converter on the OpenDocument file.  Return the
                ;; converted file.
                (org-odt-preferred-output-format
                 (or (org-odt-convert target org-odt-preferred-output-format)
                     target))
                ;; Case 2: No further conversion.  Return exported
                ;; OpenDocument file.
                (t target))))
         (error
          ;; Cleanup work directory and work files.
          (funcall --cleanup-xml-buffers)
          (message "OpenDocument export failed: %s"
                   (error-message-string err))))))
  ;; same as the origin, put here to update the above macro.
  (defun org-odt-export-to-odt (&optional async subtreep visible-only ext-plist)
    (interactive)
    (let ((outfile (org-export-output-file-name ".odt" subtreep)))
      (if async
          (org-export-async-start (lambda (f) (org-export-add-to-stack f 'odt))
            `(expand-file-name
              (org-odt--export-wrap
               ,outfile
               (let* ((org-odt-embedded-images-count 0)
                      (org-odt-embedded-formulas-count 0)
                      (org-odt-automatic-styles nil)
                      (org-odt-object-counters nil)
                      ;; Let `htmlfontify' know that we are interested in
                      ;; collecting styles.
                      (hfy-user-sheet-assoc nil))
                 ;; Initialize content.xml and kick-off the export
                 ;; process.
                 (let ((out-buf
                        (progn
                          (require 'nxml-mode)
                          (let ((nxml-auto-insert-xml-declaration-flag nil))
                            (find-file-noselect
                             (concat org-odt-zip-dir "content.xml") t))))
                       (output (org-export-as
                                'odt ,subtreep ,visible-only nil ,ext-plist)))
                   (with-current-buffer out-buf
                     (erase-buffer)
                     (insert output)))))))
        (org-odt--export-wrap
         outfile
         (let* ((org-odt-embedded-images-count 0)
                (org-odt-embedded-formulas-count 0)
                (org-odt-automatic-styles nil)
                (org-odt-object-counters nil)
                ;; Let `htmlfontify' know that we are interested in collecting
                ;; styles.
                (hfy-user-sheet-assoc nil))
           ;; Initialize content.xml and kick-off the export process.
           (let ((output (org-export-as 'odt subtreep visible-only nil ext-plist))
                 (out-buf (progn
                            (require 'nxml-mode)
                            (let ((nxml-auto-insert-xml-declaration-flag nil))
                              (find-file-noselect
                               (concat org-odt-zip-dir "content.xml") t)))))
             (with-current-buffer out-buf (erase-buffer) (insert output))))))))
  (use-package org-bullets
    :init
    (setq org-bullets-bullet-list
          '(;;; Large
            "✸"
            "◉"
            "○"
            "▶"
            "✿"
            ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
    ;;; Small
            ;; ► • ★ ▸
            )))
  )

(provide 'init-org)
;;; init-org.el ends here
