;;; init-helm-flyspell.el --- Summary
;;; Commentary:
;; The Aspell is the best interactive spell checkers, but not maintained.
;; The Hunspell is not as good as Aspell, but used widely and actively maintained.
;; Aspell v.s. Hunspell : better word correction, support for CamelWords.

;;; Code:
(use-package helm-flyspell
  :bind (("C-c s t" . flyspell-toggle-dwim)
         ("C-c s b" . flyspell-buffer-dwim))
  :commands (flyspell-toggle-dwim flyspell-buffer-dwim)
  :preface
  (defun flyspell-toggle-dwim ()
    "Use `flyspell-mode' for ordinary files, and `flyspell-prog-mode'

for programming files."
    (interactive)
    (require 'helm-flyspell)
    (if flyspell-mode
        (progn
          (message "Turn off flyspell...")
          (flyspell-mode -1)
          (ispell-kill-ispell t))
      (if (memq major-mode
                '(lisp-mode
                  emacs-lisp-mode scheme-mode clojure-mode ruby-mode
                  yaml-mode python-mode shell-mode php-mode css-mode haskell-mode
                  caml-mode nxml-mode crontab-mode perl-mode tcl-mode js2-mode))
          (flyspell-prog-mode)
        (flyspell-mode 1))
      (flyspell-buffer)))

  (defun flyspell-buffer-dwim ()
    "When `flyspell-mode' is on, check the buffer, otherwise, turn on flyspell."
    (interactive)
    (if flyspell-mode
        (flyspell-buffer)
      (flyspell-toggle-dwim)))

  :config
  (use-package flyspell-lazy
    :init (flyspell-lazy-mode 1))

  (use-package flyspell :ensure nil
    :init
    (setq flyspell-issue-welcome-flag nil)
    (setq flyspell-issue-message-flag nil)
    (bind-key   "C-," 'flyspell-goto-next-error flyspell-mode-map)
    (bind-key   "C-." 'helm-flyspell-correct    flyspell-mode-map)
    (unbind-key "C-;" flyspell-mode-map)

    (cond
     ((executable-find "aspell")   (setq ispell-program-name "aspell"))
     ((executable-find "hunspell") (setq ispell-program-name "hunspell"))
     (t                            (setq ispell-program-name nil)))

    (defun flyspell-set-camel-check-args (&optional camel-flag)
      "if CAMEL-FLAG is true, spell check the CamelCase words. Only support Aspell."
      (cond
       ((string-match "aspell" ispell-program-name)
        ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
        (if camel-flag
            (setq ispell-extra-args (list "--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=5" "--run-together-min=2"))
          (setq ispell-extra-args (list "--sug-mode=ultra" "--lang=en_US"))))
       ((string-match "hunspell" ispell-program-name)
        (setq ispell-extra-args nil))))
    ;; Camel check is good, but word correction is painful. Don't use it!!!
    ;; Need to use (ispell-kill-ispell t) to kill before process, it's slow.
    (flyspell-set-camel-check-args nil)

    (setq ispell-local-dictionary "en_US")
    (setq ispell-local-dictionary-alist
          '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))

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
            (setq verified-p (string= (thing-at-point 'word) "value")))))
        verified-p))

    (put 'web-mode 'flyspell-mode-predicate 'web-mode-flyspell-verify))

  ;; NOTICE: Hard code this function to prevent hanging during compilation.
  (defun ispell-check-version (&optional interactivep)
    (interactive "p")
    "@(#) International Ispell Version 3.1.20 (but really Aspell 0.60.7-20131207), ispell.el 3.6 - 7-Jan-2003"))

(provide 'init-helm-flyspell)
;;; init-helm-flyspell.el ends here