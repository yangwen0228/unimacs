;;; init-switch-window.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package switch-window
  :bind (("C-c C-b C-p" . buf-move-up)
         ("C-c C-b C-n" . buf-move-dow)
         ("C-c C-b C-b" . buf-move-left)
         ("C-c C-b C-f" . buf-move-right)))

(provide 'init-switch-window)
;;; init-switch-window.el ends here