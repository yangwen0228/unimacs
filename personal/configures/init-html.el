;;; init-html.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package html-mode
  :ensure nil
  :mode (("\\.(jsp|tmpl)\\'" . html-mode))
  :config
  (use-package tidy
    :init
    (add-hook 'html-mode-hook (lambda () (tidy-build-menu html-mode-map))))

  (use-package tagedit
    :disabled t
    :init
    (eval-after-load 'sgml-mode
      (add-hook 'sgml-mode-hook (lambda () (tagedit-mode 1))))
    ))

(provide 'init-html)
;;; init-html.el ends here
