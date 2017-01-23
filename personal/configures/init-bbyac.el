;;; init-bbyac.el --- Summary
;;; Commentary:
;; Complete lines, symbols.

;;; Code:
(use-package bbyac
  :bind (("M-s l" . bbyac-expand-lines)
         ("M-s w" . bbyac-expand-symbols)))

(provide 'init-bbyac)
;;; init-bbyac.el ends here