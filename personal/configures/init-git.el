;;; init-git.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package magit
  :commands magit-mode
  :init
  (setq magit-process-popup-time 10)
  :config
  (defadvice magit-status (around magit-fullscreen activate)
         (window-configuration-to-register :magit-fullscreen)
         ad-do-it
         (delete-other-windows))
  (defun magit-quit-session ()
         "Restores the previous window configuration and kills the magit buffer"
         (interactive)
         (kill-buffer)
         (jump-to-register :magit-fullscreen))
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

(provide 'init-git)
;;; init-git.el ends here
