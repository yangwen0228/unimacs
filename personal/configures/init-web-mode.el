;;; init-web-mode.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package web-mode
  :mode (("\\.phtml\\'"    . web-mode)
         ("\\.wp\\'"       . web-mode)
         ("\\.tmpl\\'"     . web-mode)
         ("\\.php\\'"      . web-mode)
         ("\\.hbs\\'"      . web-mode)
         ("\\.tpl\\'"      . web-mode)
         ("\\.jsp\\'"      . web-mode)
         ("\\.jsx\\'"      . web-mode)
         ("\\.as[cp]x\\'"  . web-mode)
         ("\\.erb\\'"      . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'"   . web-mode)
         ("\\.ftl\\'"      . web-mode)
         ("\\.html?\\'"    . web-mode)
         ("\\.xul?\\'"     . web-mode))
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) (append company-backends '((company-web-html company-web-jade company-yasnippet))))
              ;; (flyspell-mode 1)
              (remove-hook 'yas-after-exit-snippet-hook
                           'web-mode-yasnippet-exit-hook t)
              (remove-hook 'yas/after-exit-snippet-hook
                           'web-mode-yasnippet-exit-hook t)
              ))
  ;; make org-mode export fail, I use evil and evil-matchit
  ;; to select text, so expand-region.el is not used
  (remove-hook 'web-mode-hook 'er/add-web-mode-expansions)
  (setq web-mode-enable-auto-closing t) ; enable auto close tag in text-mode
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-imenu-regexp-list
        '(("<\\(h[1-9]\\)\\([^>]*\\)>\\([^<]*\\)" 1 3 ">" nil)
          ("^[ \t]*<\\([@a-z]+\\)[^>]*>? *$" 1 " id=\"\\([a-zA-Z0-9_]+\\)\"" "#" ">")
          ("^[ \t]*<\\(@[a-z.]+\\)[^>]*>? *$" 1 " contentId=\"\\([a-zA-Z0-9_]+\\)\"" "=" ">")
          ;; angular imenu
          (" \\(ng-[a-z]*\\)=\"\\([^\"]+\\)" 1 2 "="))))

(provide 'init-web-mode)
;;; init-web-mode.el ends here
