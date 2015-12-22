;;; init-origami.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(require 'origami)
(global-origami-mode t)

(define-key origami-mode-map (kbd "C-z m") 'origami-close-all-nodes)
(define-key origami-mode-map (kbd "C-z c") 'origami-close-node)
(define-key origami-mode-map (kbd "C-z C") 'origami-close-node-recursively)
(define-key origami-mode-map (kbd "C-z r") 'origami-open-all-nodes)
(define-key origami-mode-map (kbd "C-z o") 'origami-open-node)
(define-key origami-mode-map (kbd "C-z O") 'origami-open-node-recursively)

(provide 'init-origami)
;;; init-origami.el ends here