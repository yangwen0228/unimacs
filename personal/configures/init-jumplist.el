;;; init-jumplist.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package jumplist
  :init
  ;; THe keybind C-, and C-. conflict with InputMethod.
  (global-set-key (kbd "M-,") 'jumplist-previous)
  (global-set-key (kbd "M-.") 'jumplist-next)
  ;; add a point when no redo or undo points.
  (defun jumplist-previous ()
    "Jump back."
    (interactive)
    (if (or (not jumplist--list)
            (and (not (jumplist--first?))
                 (jumplist--last?)))
        (progn
          (jumplist--set) ; add a point when no undo points.
          (message "No further undo point."))
      (if jumplist-ex-mode
          (unless jumplist--jumping
            (jumplist--set)
            (setq jumplist--jumping 't)))
      (jumplist--inc-idx)
      (let ((buff (nth jumplist--idx jumplist--list)))
        (jumplist--do-jump buff))))
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
     '(unimacs-move-beginning-of-line
       end-of-visual-line
       beginning-of-defun end-of-defun
       sp-forward-sexp sp-backward-sexp
       helm-swoop helm-imenu helm-find-files helm-multi-files
       helm-projectile-switch-project helm-projectile-find-file
       find-function find-variable
       mark-defun mark-whole-buffer
       avy-goto-char avy-goto-char-2
       helm-gtags-find-pattern helm-gtags-find-tag-adapter helm-gtags-find-rtag-adapter
       helm-ag-select-directory
       ensime-edit-definition
       ensime-edit-definition-with-fallback
       isearch-forward
       end-of-buffer beginning-of-buffer))
   '(jumplist-ex-mode t))
  )

(provide 'init-jumplist)
;;; init-jumplist.el ends here
