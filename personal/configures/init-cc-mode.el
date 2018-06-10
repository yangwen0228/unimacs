;;; init-cc-mode.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package cc-mode
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'"                   . c-mode)
         ("\\.mm\\'"                  . c++-mode))
  :preface
  (defun c-wx-lineup-topmost-intro-cont (langelem)
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward "EVT_" (line-end-position) t)
          'c-basic-offset
        (c-lineup-topmost-intro-cont langelem))))
  (defconst my-c-style
    `((c-recognize-knr-p . nil)
      (c-enable-xemacs-performance-kludge-p . t) ; speed up indentation in XEmacs
      (c-basic-offset . 4)
      (indent-tabs-mode . nil)
      (c-comment-only-line-offset . 0)
      (c-hanging-braces-alist . ((defun-open after)
                                 (defun-close before after)
                                 (class-open after)
                                 (class-close before after)
                                 (inexpr-class-open after)
                                 (inexpr-class-close before)
                                 (namespace-open after)
                                 (inline-open after)
                                 (inline-close before after)
                                 (block-open after)
                                 (block-close . c-snug-do-while)
                                 (extern-lang-open after)
                                 (extern-lang-close after)
                                 (statement-case-open after)
                                 (substatement-open after)))
      (c-hanging-colons-alist . ((case-label)
                                 (label after)
                                 (access-label after)
                                 (member-init-intro before)
                                 (inher-intro)))
      (c-hanging-semi&comma-criteria
       . (c-semi&comma-no-newlines-for-oneline-inliners
          c-semi&comma-inside-parenlist
          c-semi&comma-no-newlines-before-nonblanks))
      (c-indent-comments-syntactically-p . t)
      (comment-column . 40)
      (c-indent-comment-alist . ((other . (space . 2))))
      (c-cleanup-list . (brace-else-brace
                         bracpe-elseif-brace
                         brace-catch-brace
                         empty-defun-braces
                         defun-close-semi
                         list-close-comma
                         scope-operator))
      (c-offsets-alist . ((arglist-intro ++)
                          (arglist-close . c-lineup-arglist)
                          (brace-list-intro . +)
                          (func-decl-cont . ++)
                          (member-init-intro . ++)
                          (inher-intro . ++)
                          (comment-intro . 0)
                          (topmost-intro . 0)
                          (block-open . 0)
                          (inline-open . 0)
                          (substatement-open . 0)
                          (statement-cont
                           .
                           (,(when (fboundp 'c-no-indent-after-java-annotations)
                               'c-no-indent-after-java-annotations)
                            ,(when (fboundp 'c-lineup-assignments)
                               'c-lineup-assignments)
                            ++))
                          (label . /)
                          (case-label . +)
                          (statement-case-open . +)
                          (statement-case-intro . +) ; case w/o {
                          (access-label . /)
                          (innamespace . 0))))
    "Google C/C++ Programming Style.")
  (defun my-set-c-style ()
    "Set the current buffer's c-style to Google C/C++ Programming
  Style. Meant to be added to `c-mode-common-hook'."
    (interactive)
    (make-local-variable 'c-tab-always-indent)
    (setq c-tab-always-indent t)
    (c-add-style "my-c-style" my-c-style t))

  :config
  ;; donot use c-mode-common-hook or cc-mode-hook because many major-modes use this hook
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (modify-syntax-entry ?_ "w")
                (my-set-c-style))))

  (unimacs-company-define-backends
   '((c-mode c++-mode objc-mode) . ((company-irony :with company-yasnippet)
                                    (company-dabbrev-code :with company-dabbrev company-yasnippet)
                                    company-c-headers company-files)))
  ;; (use-package google-c-style)
  ;; (add-hook 'c-mode-common-hook 'google-set-c-style)
  ;; (add-hook 'c-mode-common-hook 'google-make-newline-indent)
  )

(provide 'init-cc-mode)
;;; init-cc-mode.el ends here
