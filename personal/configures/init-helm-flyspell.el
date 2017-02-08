;;; init-helm-flyspell.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package helm-flyspell
  :bind (("C-c s t" . flyspell-dwim)
         ("C-c s b" . flyspell-buffer-dwim)
         ("C-c s c" . helm-flyspell-correct))
  :commands (helm-flyspell-correct flyspell-dwim flyspell-buffer-dwim)
  :preface
  (defun flyspell-dwim ()
    "Use `flyspell-mode' for ordinary files, and `flyspell-prog-mode'

for programming files."
    (interactive)
    (require 'helm-flyspell)
    (if flyspell-mode
        (flyspell-mode -1)
      (if (memq major-mode
                '(lisp-mode
                  emacs-lisp-mode scheme-mode clojure-mode ruby-mode
                  yaml-mode python-mode shell-mode php-mode css-mode haskell-mode
                  caml-mode nxml-mode crontab-mode perl-mode tcl-mode js2-mode))
          (flyspell-prog-mode)
        (flyspell-mode 1))
      (flyspell-buffer)))

  (defun flyspell-buffer-dwim ()
    "When `flyspell-mode' is on, check the buffer, otherwise, do nothing."
    (interactive)
    (require 'helm-flyspell)
    (when flyspell-mode
      (flyspell-buffer)))

  :config
  (setq flyspell-issue-message-flag nil)
  (unbind-key "C-;" flyspell-mode-map)
  ;; (unbind-key "C-." flyspell-mode-map)
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
        )))

  ;; flyspell set up for web-mode
  (defun web-mode-flyspell-verify ()
    (let ((face (get-text-property (- (point) 1) 'face)) verified-p)
      (cond
       ((not (memq face '(web-mode-html-attr-value-face
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
                          web-mode-block-control-face)))
        (setq verified-p t))
       ((memq face '(web-mode-html-attr-value-face))
        (save-excursion
          (search-backward-regexp "=['\"]" (line-beginning-position) t)
          (backward-char)
          (setq verified-p (string= (thing-at-point 'word) "value"))))
       )
      verified-p))

  (put 'web-mode 'flyspell-mode-predicate 'web-mode-flyspell-verify)

  (add-hook 'nxml-mode-hook
            (lambda ()
              (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)))
  )

(provide 'init-helm-flyspell)
;;; init-helm-flyspell.el ends here