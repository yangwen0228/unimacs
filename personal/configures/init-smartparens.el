;;; init-smartparens.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package smartparens
  :bind (("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)
         ("C-M-n" . sp-next-sexp)
         ("C-M-p" . sp-previous-sexp)
         ("M-[" .   sp-unwrap-sexp)
         ("M-]" .   sp-backward-unwrap-sexp)
         ("C-M-{" . sp-rewrap-sexp))
  :init
  (use-package smartparens-config :ensure nil)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode t)
  (setq sp-navigate-consider-sgml-tags '(html-mode nxml-mode web-mode xml-mode))
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (add-hook 'lisp-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)
  :diminish (smartparens-mode smartparens-global-mode))

(provide 'init-smartparens)
;;; init-smartparens.el ends here