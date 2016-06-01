;;; init-jumplist.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package jumplist
  :init
  (global-set-key (kbd "C-,") 'jumplist-previous)
  (global-set-key (kbd "C-.") 'jumplist-next)
  (custom-set-variables
   '(jumplist-hook-commands
     '(helm-swoop
       helm-imenu helm-for-files helm-projectile-switch-project
       helm-projectile-find-file
       ido-find-file
       find-file find-function find-variable
       dired-jump avy-goto-char
       helm-gtags-find-pattern helm-gtags-find-tag helm-gtags-find-rtag-adapter
       helm-ag-select-directory
       isearch-forward end-of-buffer beginning-of-buffer))
   '(jumplist-ex-mode t))
  )

(provide 'init-jumplist)
;;; init-jumplist.el ends here
