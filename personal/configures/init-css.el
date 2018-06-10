;;; init-css.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package css-mode :ensure nil
  ;; Colourise CSS colour literals
  :hook ((css-mode html-mode sass-mode) . rainbow-mode)
  :config
  (defun my-css-imenu-make-index ()
    (save-excursion
      (imenu--generic-function '((nil "^ *\\([^ ]+\\) *{ *$" 1)))))
  (add-hook 'css-mode-hook
            (lambda () (setq imenu-create-index-function 'my-css-imenu-make-index)))
  (add-hook 'scss-mode-hook
            (lambda () (setq imenu-create-index-function 'my-css-imenu-make-index))))

(provide 'init-css)
;;; init-css.el ends here
