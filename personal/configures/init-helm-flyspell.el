;;; init-helm-flyspell.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package helm-flyspell
  :bind (("C-c s t" . flyspell-mode)
         ("C-c s b" . flyspell-buffer)
         ("C-c s c" . helm-flyspell-correct))
  :commands (helm-flyspell-correct flyspell-buffer)
  :init
  (add-hook 'flyspell-mode-hook (lambda () (when flyspell-mode (flyspell-buffer))))
  (use-package flyspell-lazy
    :init (flyspell-lazy-mode 1))
  (use-package ispell :ensure nil
    :init
    (defun flyspell-detect-ispell-args (&optional RUN-TOGETHER)
      "if RUN-TOGETHER is true, spell check the CamelCase words"
      (let (args)
        (cond
         ((string-match "aspell" ispell-program-name)
          ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
          (setq args (list "--sug-mode=ultra" "--lang=en_US"))
          (if RUN-TOGETHER
              (setq args (append args '("--run-together" "--run-together-limit=5" "--run-together-min=2")))))
         ((string-match "hunspell" ispell-program-name)
          (setq args nil)))
        args))
    (cond
     ((executable-find "aspell")   (setq ispell-program-name "aspell"))
     ((executable-find "hunspell") (setq ispell-program-name "hunspell"))
     (t                            (setq ispell-program-name nil)))
    (setq ispell-local-dictionary "en_US")
    (setq ispell-local-dictionary-alist
          '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))

    ;; ispell-cmd-args is useless, it's the list of *extra* command line arguments we will append to the ispell process when ispell-send-string()
    ;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
    (setq ispell-extra-args (flyspell-detect-ispell-args t))
    ;; (setq ispell-cmd-args (flyspell-detect-ispell-args))
    (defadvice ispell-word (around my-ispell-word activate)
      (let ((old-ispell-extra-args ispell-extra-args))
        (ispell-kill-ispell t)
        (setq ispell-extra-args (flyspell-detect-ispell-args))
        ad-do-it
        (setq ispell-extra-args old-ispell-extra-args)
        (ispell-kill-ispell t)
        ))
    )

  ;; (unbind-key "C-." flyspell-mode-map)
  ;; flyspell set up for web-mode
  (defun web-mode-flyspell-verify ()
    (let ((f (get-text-property (- (point) 1) 'face))
          rlt)
      (cond
       ((not (memq f '(web-mode-html-attr-value-face
                       web-mode-html-tag-face
                       web-mode-html-attr-name-face
                       web-mode-constant-face
                       web-mode-doctype-face
                       web-mode-keyword-face
                       web-mode-comment-face ;; focus on get html label right
                       web-mode-function-name-face
                       web-mode-variable-name-face
                       web-mode-css-property-name-face
                       web-mode-css-selector-face
                       web-mode-css-color-face
                       web-mode-type-face
                       web-mode-block-control-face
                       )
                   ))
        (setq rlt t))
       ((memq f '(web-mode-html-attr-value-face))
        (save-excursion
          (search-backward-regexp "=['\"]" (line-beginning-position) t)
          (backward-char)
          (setq rlt (string= (thing-at-point 'word) "value"))
          ))
       (t t))
      rlt
      ))

  (put 'web-mode 'flyspell-mode-predicate 'web-mode-flyspell-verify)

  (setq flyspell-issue-message-flag nil)

  ;;----------------------------------------------------------------------------
  ;; Add spell-checking in comments for all programming language modes
  ;;----------------------------------------------------------------------------
  (dolist (hook '(lisp-mode-hook
                  emacs-lisp-mode-hook
                  scheme-mode-hook
                  clojure-mode-hook
                  ruby-mode-hook
                  yaml-mode
                  python-mode-hook
                  shell-mode-hook
                  php-mode-hook
                  css-mode-hook
                  haskell-mode-hook
                  caml-mode-hook
                  nxml-mode-hook
                  crontab-mode-hook
                  perl-mode-hook
                  tcl-mode-hook
                  js2-mode-hook))
    (add-hook hook 'flyspell-prog-mode))
  (add-hook 'nxml-mode-hook
            (lambda ()
              (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)))
  )

(provide 'init-helm-flyspell)
;;; init-helm-flyspell.el ends here