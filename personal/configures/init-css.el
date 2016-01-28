;; Colourise CSS colour literals
;; web-mode does not like rainbow-mode
(dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
  (add-hook hook 'rainbow-mode)
  )
(defun my-css-imenu-make-index ()
  (save-excursion
    (imenu--generic-function '((nil "^ *\\([^ ]+\\) *{ *$" 1)))))

(add-hook 'css-mode-hook
          (lambda ()
              (setq imenu-create-index-function 'my-css-imenu-make-index)))

(add-hook 'scss-mode-hook
          (lambda ()
              (setq imenu-create-index-function 'my-css-imenu-make-index)))

(provide 'init-css)
