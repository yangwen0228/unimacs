;;; init-origami.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package origami
  :bind-keymap ("C-z" . origami-mode-map)
  :config
  (global-origami-mode t)
  (add-to-list 'origami-parser-alist '(tcl-mode . origami-c-style-parser))
  (define-key origami-mode-map (kbd "C-z m") 'origami-close-all-nodes)
  (define-key origami-mode-map (kbd "C-z c") 'origami-close-node)
  (define-key origami-mode-map (kbd "C-z C") 'origami-close-node-recursively)
  (define-key origami-mode-map (kbd "C-z r") 'origami-open-all-nodes)
  (define-key origami-mode-map (kbd "C-z o") 'origami-open-node)
  (define-key origami-mode-map (kbd "C-z O") 'origami-open-node-recursively)
  )

(provide 'init-origami)
;;; init-origami.el ends here