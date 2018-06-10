;;; init-emmet.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package emmet-mode
  :hook ((sgml-mode web-mode css-mode nxml-mode) . emmet-mode))

(provide 'init-emmet)
;;; init-emmet.el ends here