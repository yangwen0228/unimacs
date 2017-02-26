;;; init-helm-imenu.el --- Summary
;;; Commentary:
;; imenu completing UI using helm.

;;; Code:
(use-package helm-imenu :ensure nil
  :bind ("C-c i" . helm-imenu)
  :config
  (use-package imenu :ensure nil
    ;; type name must be capital, like: Use-package. Cannot use use-package.
    :init
    (add-to-list 'lisp-imenu-generic-expression
                 (list "Use-package"
                       (concat
                        "^\\s-*(use-package\\s-+" ; definition
                        "\\([-A-Za-z0-9_:+*]+\\)" ; package name
                        ) 1))))

(provide 'init-helm-imenu)
;;; init-helm-imenu.el ends here