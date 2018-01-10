;;; init-java.el --- Summary
;;; Commentary:
;; Java development.

;;; Code:
(use-package meghanada
  :defer t
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              (meghanada-mode t)
              ;; (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
              ))

  :config
  (use-package realgud)
  (setq indent-tabs-mode nil)
  ;; (setq tab-width 4)
  ;; (setq c-basic-offset 4)
  (setq meghanada-debug t)
  (setq meghanada-server-remote-debug t)
  (setq meghanada-javac-xlint "-Xlint:all,-processing")
  :bind
  (:map meghanada-mode-map
        ("C-S-t" . meghanada-switch-testcase)
        ("M-RET" . meghanada-local-variable)
        ("C-M-." . helm-imenu)
        ("M-r" . meghanada-reference)
        ("M-t" . meghanada-typeinfo)
        ("C-z" . hydra-meghanada/body))
  :commands
  (meghanada-mode))

(defhydra hydra-meghanada (:hint nil :exit t)
  "
^Edit^                           ^Tast or Task^
^^^^^^-------------------------------------------------------
_f_: meghanada-compile-file      _m_: meghanada-restart
_c_: meghanada-compile-project   _t_: meghanada-run-task
_o_: meghanada-optimize-import   _j_: meghanada-run-junit-test-case
_s_: meghanada-switch-test-case  _J_: meghanada-run-junit-class
_v_: meghanada-local-variable    _R_: meghanada-run-junit-recent
_i_: meghanada-import-all        _r_: meghanada-reference
_g_: magit-status                _T_: meghanada-typeinfo
_l_: helm-ls-git-ls
_q_: exit
"
  ("f" meghanada-compile-file)
  ("m" meghanada-restart)

  ("c" meghanada-compile-project)
  ("o" meghanada-optimize-import)
  ("s" meghanada-switch-test-case)
  ("v" meghanada-local-variable)
  ("i" meghanada-import-all)

  ("g" magit-status)
  ("l" helm-ls-git-ls)

  ("t" meghanada-run-task)
  ("T" meghanada-typeinfo)
  ("j" meghanada-run-junit-test-case)
  ("J" meghanada-run-junit-class)
  ("R" meghanada-run-junit-recent)
  ("r" meghanada-reference)

  ("q" exit)
  ("z" nil "leave"))

(use-package ensime
  :init
  ;; (add-hook 'java-mode-hook (lambda () (ensime-mode 1)))
  (put 'ensime-auto-generate-config 'safe-local-variable #'booleanp)
  (setq
   ensime-use-helm t
   ensime-startup-notification nil
   ensime-startup-snapshot-notification nil)
  (add-hook 'ensime-mode-hook (lambda ()
                                (unbind-key "M-n" ensime-mode-map)
                                (unbind-key "M-p" ensime-mode-map)
                                (unbind-key "M-," ensime-mode-map)
                                (unbind-key "M-." ensime-mode-map)
                                (bind-key "M-t" 'ensime-edit-definition-with-fallback ensime-mode-map)
                                ))
  :config
  (require 'ensime-generate)
  (defun ensime-edit-definition-with-fallback (arg)
    "Variant of `ensime-edit-definition' with gtags if ENSIME is not available."
    (interactive "P")
    (unless (and (ensime-connection-or-nil)
                 (ensime-edit-definition arg))
      (call-interactively 'helm-gtags-find-tag)))
  ;; already PR
  (defun ensime-get-completions-async
      (max-results case-sense callback)
    (ensime-rpc-async-completions-at-point
     max-results case-sense
     (lexical-let ((continuation callback))
       (lambda (info)
         (let* (name
                (prefix (ensime-completion-prefix-at-point))
                (candidates (remove-if
                             '(lambda (candidate)
                                (setq name (getf candidate :name))
                                (or (string-match "\\$" name)
                                    (not (string-prefix-p prefix name))))
                             (plist-get info :completions)))
                (candidates (ensime--annotate-completions candidates)))
           (funcall continuation candidates))))))

  ;;  (defun ensime-computed-point ()
  ;;    "Subtract one to convert to 0-indexed buffer offsets.
  ;; Additionally, in buffers with windows-encoded line-endings,
  ;; add the appropriate number of CRs to compensate for characters
  ;; that are hidden by Emacs."
  ;;    (let ((utf8-count 0)
  ;;          (pos (point)))
  ;;      (goto-char (point-min))
  ;;      (while (not (= (point) pos))
  ;;        (unless (string= "ascii" (char-charset (char-after)))
  ;;          (setq utf8-count (1+ utf8-count)))
  ;;        (goto-char (1+ (point))))
  ;;      (ensime-externalize-offset (+ (point) (/ (1+ utf8-count) 2)))))

  ;; company: If ensime is on, use ensime and yasnippet. Otherwise, use dabbrev and yasnippet.
  (setq ensime-company-case-sensitive t)
  (unimacs-company-define-backends
   '((ensime-mode) . ((ensime-company :with company-yasnippet company-files)
                      (company-dabbrev-code :with company-dabbrev company-yasnippet company-files)
                      )))

  (bind-key "C-<tab>" 'company-dabbrev-code ensime-mode-map)
  (bind-key "." 'ensime-completing-dot ensime-mode-map)
  ;; Interactive commands
  (defun ensime-completing-dot ()
    "Insert a period and show company completions."
    (interactive "*")
    (unless (= (char-before) ?.)
      (when (s-matches? (rx (+ (not space)))
                        (buffer-substring (line-beginning-position) (point)))
        (delete-horizontal-space t))
      (cond ((not (and (ensime-connected-p) ensime-completion-style))
             (insert "."))
            ((eq ensime-completion-style 'company)
             (ensime-completing-dot-company)))))

  (defun ensime-completing-dot-company ()
    (cond (company-backend
           (company-complete-selection)
           (ensime-completing-dot))
          (t
           (insert ".")
           (call-interactively 'ensime-company)
           )))

  ;; eldoc
  (setq ensime-eldoc-hints 'error
        ensime-search-interface 'helm)

  (require 'ensime-expand-region)
  (add-hook 'git-timemachine-mode-hook (lambda () (ensime-mode 0)))

  (bind-key "s-n" 'ensime-search ensime-mode-map)
  (bind-key "s-t" 'ensime-print-type-at-point ensime-mode-map))

;; java-mode is inside cc-mode, must use cc-mode
(use-package cc-mode :ensure nil
  :mode ("\\.java\\'" . java-mode)
  :config
  (when (require 'cc-mode nil t)
    (defun c-mode-newline-comments ()
      "Newline with indent and preserve multiline comments."
      (interactive)
      (c-indent-new-comment-line)
      (indent-according-to-mode))
    (bind-key "RET" 'c-mode-newline-comments java-mode-map))
  (add-to-list 'cc-imenu-java-generic-expression
               (list "Class"
                     (concat
                      "\\s-+class\\s-+" ; definition
                      "\\([-A-Za-z0-9_:+*]+\\)" ; class name
                      )
                     1))

  (bind-key "C-c c" 'sbt-command java-mode-map)
  (bind-key "C-c e" 'next-error java-mode-map))

(use-package jdecomp
  :commands jdecomp-mode
  :config
  (customize-set-variable 'jdecomp-decompiler-type 'cfr)
  (customize-set-variable 'jdecomp-decompiler-paths
                          '((cfr . "~/Downloads/cfr_0_123.jar")
                            (fernflower . "/Applications/IntelliJ IDEA.app/Contents/plugins/java-decompiler/lib/java-decompiler.jar"))))

(use-package scala-mode
  :mode ("\\.scala\\'" . scala-mode)
  :init
  (setq
   scala-indent:use-javadoc-style t
   scala-indent:align-parameters t)
  (add-hook 'scala-mode-hook (lambda ()
                               (ensime-mode +1)
                               (scala-mode:goto-start-of-code)))
  :config
  ;; prefer smartparens for parens handling
  (remove-hook 'post-self-insert-hook
               'scala-indent:indent-on-parentheses)

  (when (require 'smartparens-mode nil t)
    (sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair 'scala-mode "{" nil
                   :post-handlers '(("||\n[i]" "RET")
                                    ("| " "SPC")
                                    fommil-sp-wrap-with-brackets)))

  (defun scala-mode-newline-comments ()
    "Custom newline appropriate for `scala-mode'."
    ;; shouldn't this be in a post-insert hook?
    (interactive)
    (newline-and-indent)
    (scala-indent:insert-asterisk-on-multiline-comment))

  (bind-key "RET" 'scala-mode-newline-comments scala-mode-map)

  ;; i.e. bypass company-mode
  (bind-key "C-<tab>" 'dabbrev-expand scala-mode-map)

  (bind-key "C-c c" 'sbt-command scala-mode-map)
  (bind-key "C-c e" 'next-error scala-mode-map))

;; build tools
(use-package sbt-mode
  :commands sbt-command
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

(use-package gradle-mode
  :commands gradle-mode)

(use-package groovy-mode
  :mode ("\\.gradle\\'" . groovy-mode)
  :config
  (use-package groovy-imports)
  (unimacs-company-define-backends
   '((groovy-mode) . ((company-dabbrev-code :with company-dabbrev company-yasnippet)))))

(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode)
  :commands restclient-mode
  :config
  (use-package company-restclient)
  (unimacs-company-define-backends
   '((restclient-mode) . ((company-restclient :with company-dabbrev company-dabbrev company-yasnippet) company-files))))

(provide 'init-java)
;;; init-java.el ends here
