;;; init-auto-yasnippet.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package auto-yasnippet
  :bind (("C-c y c" . aya-create)
         ("C-c y e" . aya-expand)
         ("C-c y o" . aya-open-line))
  :config
  (defadvice aya-expand (after aya-expand-indent activate)
    (unimacs-indent-current-line-or-region)
      ))

(provide 'init-auto-yasnippet)
;;; init-auto-yasnippet.el ends here
