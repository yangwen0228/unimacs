;;; init-smartparens.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(require 'smartparens)
(require 'smartparens-config)

(smartparens-global-mode 1)
(show-smartparens-global-mode t)
(setq sp-navigate-consider-sgml-tags '(html-mode nxml-mode web-mode xml-mode))

(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)
(define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)

(add-hook 'lisp-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

(defun unimacs-create-sp-wrapper (s)
  "Create a wrapper function for smartparens using S."
  `(lambda (&optional arg)
     (interactive "P")
     (sp-wrap-with-pair ,s)))


(provide 'init-smartparens)
;;; init-smartparens.el ends here