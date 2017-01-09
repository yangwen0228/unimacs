;;; init-iedit.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package iedit
  :bind (("C-;" . iedit-mode)
         ("C-:" . iedit-in-function))
  :preface
  (defun iedit-in-function ()
    "Iedit only in function."
    (interactive)
    (iedit-mode 0))
  :config
  (define-key iedit-mode-occurrence-keymap (kbd "M-n") 'iedit-next-occurrence)
  (define-key iedit-mode-occurrence-keymap (kbd "M-p") 'iedit-prev-occurrence))

(provide 'init-iedit)
;;; init-iedit.el ends here