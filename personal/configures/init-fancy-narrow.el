;;; init-fancy-narrow.el --- Summary
;;; Commentary:
;; The page is separate by pagebreak ^L. Maybe we can change this to text in current window.

;;; Code:
(use-package fancy-narrow
  :bind (("C-x n n" . fancy-narrow-to-region)
         ("C-x n w" . fancy-widen)
         ("C-x n d" . fancy-narrow-to-defun)
         ("C-x n p" . fancy-narrow-to-page)))

(provide 'init-fancy-narrow)
;;; init-fancy-narrow.el ends here