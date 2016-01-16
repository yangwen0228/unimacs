;;; init-rainbow-delimiters.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  :config
  ;; (defface rainbow-delimiters-depth-1-face
  ;;   '((t (:foreground "#00bfff")))
  ;;   "First match paren face."
  ;;   :group 'rainbow-delimiters-faces)
  (custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "DeepSkyBlue1")))))

  )

(provide 'init-rainbow-delimiters)
;;; init-rainbow-delimiters.el ends here