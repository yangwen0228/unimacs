;;; init-header2.el --- auto create or update headers
;;; Commentary:
;; Commands (interactive functions) defined here:
;;
;;   `make-header', `make-revision', `make-divider',
;;   `make-box-comment', `update-file-header'.
;;
;; Other functions defined here:
;;
;;   `auto-make-header', `auto-update-file-header',
;;   `delete-and-forget-line', `header-AFS', `header-author',
;;   `header-blank', `header-code', `header-commentary',
;;   `header-compatibility', `header-copyright',
;;   `header-creation-date', `header-date-string',
;;   `header-description', `header-doc-url',`header-end-line',
;;   `header-eof', `header-file-name', `header-free-software',
;;   `header-history', `header-keywords', `header-lib-requires',
;;   `header-maintainer', `header-mode-line',
;;   `header-modification-author', `header-modification-date',
;;   `header-multiline', `header-pkg-requires',
;;   `header-prefix-string', `header-rcs-id', `header-rcs-log',
;;   `header-sccs', `header-shell', `header-status', `header-title',
;;   `header-toc', `header-update-count', `header-url',
;;   `header-version', `headerable-file-p', `make-box-comment',
;;   `make-divider', `make-revision', `nonempty-comment-end',
;;   `nonempty-comment-start', `register-file-header-action',
;;   `section-comment-start', `true-mode-name', `uniquify-list',
;;   `update-file-name', `update-last-modified-date',
;;   `update-last-modifier', `update-lib-requires',
;;   `update-write-count'.
;;
;; User options (variables) defined here:
;;
;;   `header-copyright-notice', `header-date-format',
;;   `header-history-label', `header-max', `make-header-hook'.

;;; Code:
;; (autoload 'auto-update-file-header "header2")
;; (add-hook 'write-file-hooks 'auto-update-file-header)
;;
;; To have Emacs add a file header whenever you create a new file in
;; some mode, put this in your init file (~/.emacs):
;;
;; (autoload 'auto-make-header "header2")
;; (add-hook 'emacs-lisp-mode-hook 'auto-make-header)
;; (add-hook 'c-mode-common-hook   'auto-make-header)
(setq make-header-hook '(
                              ;;header-mode-line
                              header-title
                              header-blank
                              ;; header-file-name
                              ;; header-description
                              ;;header-status
                              header-copyright
                              header-blank
                              header-author
                              header-url
                              header-version
                              header-keywords
                              header-free-software
                              header-blank
                              header-commentary
                              header-blank
                              ;; header-maintainer
                              ;; header-creation-date
                              ;;header-rcs-id
                              ;; header-pkg-requires
                              ;;header-sccs
                              ;; header-modification-date
                              ;; header-modification-author
                              ;; header-update-count
                              ;; header-doc-url
                              ;; header-compatibility
                              ;; header-blank
                              ;; header-lib-requires
                              ;; header-end-line
                              ;; header-history
                              ;; header-blank
                              ;; header-blank
                              ;; header-rcs-log
                              ;; header-end-line
                              header-code
                              header-end-line
                              header-blank
                              header-blank
                              header-eof
                              )
      )
(provide 'init-header2)
;;; init-header2.el ends here