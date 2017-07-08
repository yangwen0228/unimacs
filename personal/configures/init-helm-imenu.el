;;; init-helm-imenu.el --- Summary
;;; Commentary:
;; imenu completing UI using helm.

;;; Code:
(use-package helm-imenu :ensure nil
  :bind ("C-c i" . helm-imenu)
  :config
  (require 'imenu))

(use-package imenu :ensure nil
  :defer t
  ;; type name must be capital, like: Use-package. Cannot use use-package.
  :config
  (add-to-list 'lisp-imenu-generic-expression
               (list "Use-package"
                     (concat
                      "^\\s-*(use-package\\s-+" ; definition
                      "\\([-A-Za-z0-9_:+*]+\\)" ; package name
                      ) 1))
  (add-to-list 'cc-imenu-java-generic-expression
               (list "Class"
                     (concat
                      "\\s-+class\\s-+" ; definition
                      "\\([-A-Za-z0-9_:+*]+\\)" ; class name
                      ) 1)))

(provide 'init-helm-imenu)
;;; init-helm-imenu.el ends here