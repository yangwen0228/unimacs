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
      (c-offsets-alist . ((arglist-intro google-c-lineup-expression-plus-4)
                          (func-decl-cont . ++)
                          (member-init-intro . ++)
                          (inher-intro . ++)
                          (comment-intro . 0)
                          (arglist-close . c-lineup-arglist)
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

  ;; C/C++ SECTION
  (defun my-c-mode-hook ()
    ;; @see http://stackoverflow.com/questions/3509919/ \
    ;; emacs-c-opening-corresponding-header-file
    (setq c-basic-offset 4)
    (setq c-style-variables-are-local-p nil)
    ;; give me NO newline automatically after electric expressions are entered
    (setq c-auto-newline nil)


    ;;if (0)          becomes        if (0)
    ;;    {                          {
    ;;       ;                           ;
    ;;    }                          }
    (c-set-offset 'substatement-open 0)

    ;; first arg of arglist to functions: tabbed in once
    ;; (default was c-lineup-arglist-intro-after-paren)
    (c-set-offset 'arglist-intro '+)

    ;; second line of arglist to functions: tabbed in once
    ;; (default was c-lineup-arglist)
    (c-set-offset 'arglist-cont-nonempty '+)

    ;; switch/case:  make each case line indent from switch
    (c-set-offset 'case-label '+)

    ;; make the ENTER key indent next line properly
    (local-set-key "\C-m" 'newline-and-indent)

    ;; make DEL take all previous whitespace with it
    (c-toggle-hungry-state 1)

    ;; make open-braces after a case: statement indent to 0 (default was '+)
    (c-set-offset 'statement-case-open 0)

    ;; make a #define be left-aligned
    (setq c-electric-pound-behavior (quote (alignleft)))

    ;; wxwdigets stuff
    (c-set-offset 'topmost-intro-cont 'c-wx-lineup-topmost-intro-cont)

    ;; do not impose restriction that all lines not top-level be indented at least
    ;; 1 (was imposed by gnu style by default)
    (setq c-label-minimum-indentation 0)

    (my-set-c-style)
    (c-set-style "my-c-style")

    (require 'fic-mode)
    (add-hook 'c++-mode-hook 'turn-on-fic-mode)

    ;; (cppcm-reload-all)
    )
  :config
  ;; donot use c-mode-common-hook or cc-mode-hook because many major-modes use this hook
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (modify-syntax-entry ?_ "w"))
              (when (derived-mode-p 'c-mode 'c++-mode)
                (my-c-mode-hook))))

  (require 'google-c-style)
  ;; (add-hook 'c-mode-common-hook 'google-set-c-style)
  ;; (add-hook 'c-mode-common-hook 'google-make-newline-indent)
  )

(provide 'init-cc-mode)
;;; init-cc-mode.el ends here
