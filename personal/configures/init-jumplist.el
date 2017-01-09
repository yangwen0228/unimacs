;;; init-jumplist.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package jumplist
  :init
  (global-set-key (kbd "C-,") 'jumplist-previous)
  (global-set-key (kbd "C-.") 'jumplist-next)
  (defun jumplist-next ()
    "Jump forward."
    (interactive)
    (if (or (not jumplist--list)
            (jumplist--first?))
        (progn
          (jumplist--set) ; add a point when no redo points.
          (message "No further redo point."))
      (if jumplist-ex-mode
          (unless jumplist--jumping
            (jumplist--set)
            (setq jumplist--jumping 't)))
      (jumplist--dec-idx)
      (let ((buff (nth jumplist--idx jumplist--list)))
        (jumplist--do-jump buff))))

  (custom-set-variables
   '(jumplist-hook-commands
     '(helm-swoop
       helm-imenu mark-defun mark-whole-buffer
       helm-for-files helm-projectile-switch-project helm-projectile-find-file
       ido-find-file find-file find-function find-variable
       avy-goto-char avy-goto-char-2
       helm-gtags-find-pattern helm-gtags-find-tag helm-gtags-find-rtag-adapter
       helm-ag-select-directory
       isearch-forward end-of-buffer beginning-of-buffer))
   '(jumplist-ex-mode t))
  )

(provide 'init-jumplist)
;;; init-jumplist.el ends here
