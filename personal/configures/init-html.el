(require-package 'tidy)
(add-hook 'html-mode-hook (lambda () (tidy-build-menu html-mode-map)))

(require-package 'tagedit)
(eval-after-load 'sgml-mode
  (tagedit-add-paredit-like-keybindings)
  (add-hook 'sgml-mode-hook (lambda () (tagedit-mode 1))))

(add-to-list 'auto-mode-alist ("\\.(jsp|tmpl)\\'" . 'html-mode))

;; Note: ERB is configured in init-ruby-mode

(provide 'init-html)
