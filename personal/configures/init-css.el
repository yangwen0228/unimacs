;; Colourise CSS colour literals
;; web-mode does not like rainbow-mode
(dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
  (add-hook hook 'rainbow-mode)
  )

(setq-default scss-compile-at-save nil)


(eval-after-load 'auto-complete
  '(progn
     (dolist (hook '(css-mode-hook sass-mode-hook scss-mode-hook))
       (add-hook hook 'ac-css-mode-setup))))

(provide 'init-css)
