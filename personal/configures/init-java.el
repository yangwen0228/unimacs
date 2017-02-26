;;; init-java.el --- Summary
;;; Commentary:
;; Java development.

;;; Code:
(use-package eclim :disabled
  :mode ("\\.java$" . eclim-mode)
  :commands (start-eclimd)
  :config
  (require 'eclimd)
  (setq eclimd-autostart t)
  (global-eclim-mode)
  ;; (custom-set-variables
  ;;  '(eclim-eclipse-dirs '("C:/Program Files (x86)/eclipse"))
  ;;  '(eclim-executable "\"C:/Program Files (x86)/eclipse/eclim.bat\"")
  ;;  '(eclimd-executable "C:/Program Files (x86)/eclipse/eclimd.bat"))
  (custom-set-variables
   '(eclim-eclipse-dirs '("D:/portable/eclipse"))
   '(eclim-executable "\"D:/portable/eclipse/eclim.bat\"")
   '(eclimd-executable "D:/portable/eclipse/eclimd.bat"))
  (setq eclimd-wait-for-process nil
        eclimd-default-workspace "c:/Users/yangwen/workspace/"
        help-at-pt-display-when-idle t
        help-at-pt-timer-delay 0.1)

  (help-at-pt-set-timer)

  (use-package company-emacs-eclim
    :init
    (company-emacs-eclim-setup)
    (unimacs-company-define-backends
     '((java-mode) . (company-emacs-eclim company-dabbrev-code)))
    (add-to-list 'eclim--file-coding-system-mapping '("chinese-iso-8bit-dos" . "gb2312")))
  )

(use-package jdee :disabled
  :mode ("\\.java$" . jdee-mode)
  :config
  (bind-key "C-<tab>" 'jdee-complete-minibuf jdee-mode-map)
  (setq jdee-server-dir (expand-file-name "jars" unimacs-utils-dir)
        jdee-complete-function 'jdee-complete-minibuf)
  )


(defun scala-mode-newline-comments ()
  "Custom newline appropriate for `scala-mode'."
  ;; shouldn't this be in a post-insert hook?
  (interactive)
  (newline-and-indent)
  (scala-indent:insert-asterisk-on-multiline-comment))

(defun c-mode-newline-comments ()
  "Newline with indent and preserve multiline comments."
  (interactive)
  (c-indent-new-comment-line)
  (indent-according-to-mode))

(use-package cc-mode
  :ensure nil
  :config
  (bind-key "C-c c" 'sbt-command java-mode-map)
  (bind-key "C-c e" 'next-error java-mode-map)
  (bind-key "RET" 'c-mode-newline-comments java-mode-map))

(use-package scala-mode
  :defer t
  :pin melpa
  :init
  (setq
   scala-indent:use-javadoc-style t
   scala-indent:align-parameters t)
  :config

  ;; prefer smartparens for parens handling
  (remove-hook 'post-self-insert-hook
               'scala-indent:indent-on-parentheses)

  (sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair 'scala-mode "{" nil
               :post-handlers '(("||\n[i]" "RET")
                                ("| " "SPC")
                                fommil-sp-wrap-with-brackets))

  (bind-key "RET" 'scala-mode-newline-comments scala-mode-map)
  (bind-key "s-<delete>" (sp-restrict-c 'sp-kill-sexp) scala-mode-map)
  (bind-key "s-<backspace>" (sp-restrict-c 'sp-backward-kill-sexp) scala-mode-map)
  (bind-key "s-<home>" (sp-restrict-c 'sp-beginning-of-sexp) scala-mode-map)
  (bind-key "s-<end>" (sp-restrict-c 'sp-end-of-sexp) scala-mode-map)
  ;; BUG https://github.com/Fuco1/smartparens/issues/468
  ;; backwards/next not working particularly well

  ;; i.e. bypass company-mode
  (bind-key "C-<tab>" 'dabbrev-expand scala-mode-map)

  (bind-key "C-c c" 'sbt-command scala-mode-map)
  (bind-key "C-c e" 'next-error scala-mode-map))

(defun ensime-edit-definition-with-fallback (arg)
  "Variant of `ensime-edit-definition' with ctags if ENSIME is not available."
  (interactive "P")
  (unless (and (ensime-connection-or-nil)
               (ensime-edit-definition arg))
    (projectile-find-tag)))

(use-package ensime
  :defer t
  :pin melpa
  :init
  (put 'ensime-auto-generate-config 'safe-local-variable #'booleanp)
  (setq
   ensime-startup-notification nil
   ensime-startup-snapshot-notification nil)
  :config
  (require 'ensime-expand-region)
  (add-hook 'git-timemachine-mode-hook (lambda () (ensime-mode 0)))

  (bind-key "s-n" 'ensime-search ensime-mode-map)
  (bind-key "s-t" 'ensime-print-type-at-point ensime-mode-map)
  (bind-key "M-." 'ensime-edit-definition-with-fallback ensime-mode-map))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :pin melpa
  :init
  (setq
   sbt:prefer-nested-projects t
   sbt:scroll-to-bottom-on-output nil
   sbt:default-command ";compile ;test:compile ;it:compile")
  :config
  ;; WORKAROUND: https://github.com/hvesalai/sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)

  (bind-key "C-c c" 'sbt-command sbt:mode-map)
  (bind-key "C-c e" 'next-error sbt:mode-map))

(defcustom
  scala-mode-prettify-symbols
  '(("->" . ?→)
    ("<-" . ?←)
    ("=>" . ??)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("!=" . ?≠)
    ;; implicit https://github.com/chrissimpkins/Hack/issues/214
    ("+-" . ?±))
  "Prettify symbols for scala-mode.")

(add-hook 'scala-mode-hook
          (lambda ()
            (whitespace-mode-with-local-variables)
            (show-paren-mode t)
            (smartparens-mode t)
            (yas-minor-mode t)
            (git-gutter-mode t)
            (company-mode t)
            (setq prettify-symbols-alist scala-mode-prettify-symbols)
            (prettify-symbols-mode t)
            (scala-mode:goto-start-of-code)))

(add-hook 'ensime-mode-hook
          (lambda ()
            (let ((backends (company-backends-for-buffer)))
              (setq company-backends (push '(ensime-company company-yasnippet) backends)))))

(add-hook 'java-mode-hook
          (lambda ()
            (whitespace-mode-with-local-variables)
            (show-paren-mode t)
            (smartparens-mode t)
            (yas-minor-mode t)
            (git-gutter-mode t)
            (company-mode t)
            (ensime-mode t)))

(use-package ensime
  :mode ("\\.java$" . ensime)
  :config
  (use-package gradle-mode)
  (setq ensime-use-helm t)
  )

(provide 'init-java)
;;; init-java.el ends here