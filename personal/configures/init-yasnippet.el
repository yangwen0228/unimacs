;;; init-yasnippet.el --- Summary:
;;; Commentary:
;;; code:
(require 'yasnippet)

;; my private snippets
(setq my-yasnippet-dir (expand-file-name "snippets" prelude-utils-dir))
;; (setq yas-snippet-dirs (list my-yasnippet-dir))
(setq yas-snippet-dirs (list yas-installed-snippets-dir))
(if (and  (file-exists-p my-yasnippet-dir) (not (member my-yasnippet-dir yas-snippet-dirs)))
   (add-to-list 'yas-snippet-dirs my-yasnippet-dir))

(message "yas-snippet-dirs=%s" (mapconcat 'identity yas-snippet-dirs ":"))

(yas-global-mode 1)

(defun my-yas-expand ()
  "My expand specific for ftl and jsp."
  (interactive)
  (if (buffer-file-name)
      (let ((ext (car (cdr (split-string (buffer-file-name) "\\."))) )
            (old-yas-flag yas-indent-line))
        (when (or (string= ext "ftl") (string= ext "jsp"))
          (setq yas-indent-line nil))
        (yas-expand)
        ;; restore the flag
        (setq yas-indent-line old-yas-flag))
    (yas-expand)))

;; default TAB key is occupied by auto-complete
(global-set-key (kbd "C-c k") 'my-yas-expand)
;; give yas-dropdown-prompt in yas-prompt-functions a chance
;; yas-ido-prompt
;; yas-completing-prompt
(require 'dropdown-list)
(setq yas-prompt-functions '(yas-dropdown-prompt yas-completing-prompt))
;; use yas-completing-prompt when ONLY when `M-x yas-insert-snippet'
;; thanks to capitaomorte for providing the trick.
(defadvice yas-insert-snippet (around use-completing-prompt activate)
     "Use `yas-completing-prompt' for `yas-prompt-functions' but only here..."
       (let ((yas-prompt-functions '(yas-completing-prompt)))
             ad-do-it))
;; @see http://stackoverflow.com/questions/7619640/emacs-latex-yasnippet-why-are-newlines-inserted-after-a-snippet
(setq-default mode-require-final-newline nil)
(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
