;;; init-multiple-cursors.el --- Summary
;;; Commentary:
;; IMPORTANT: multiple-cursors use ~/.emacs.d/.mc-lists.el to keep choose settings.

;;; Code:
(use-package multiple-cursors
  :bind (("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-M->"   . mc/skip-to-next-like-this)
         ("C-M-<"   . mc/skip-to-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :config
  )

(provide 'init-multiple-cursors)
;;; init-multiple-cursors.el ends here
