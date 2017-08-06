;;; init-jumplist.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package jump-tree :ensure nil
  :config
  (require 'jump-tree-visualizer)
  (global-jump-tree-mode 1)
  ;; The keybind C-, and C-. conflict with InputMethod.
  (global-set-key (kbd "M-,") 'jump-tree-jump-prev)
  (global-set-key (kbd "M-.") 'jump-tree-jump-next)
  (global-set-key (kbd "C-x j") 'jump-tree-visualize)

  (setq jump-tree-pos-list-hook-commands
        '(beginning-of-buffer
          end-of-buffer backward-up-list
          beginning-of-defun end-of-defun
          unimacs-move-beginning-of-line unimacs-move-end-of-line
          unimacs-move-beginning-of-window unimacs-move-end-of-window
          helm-swoop helm-imenu helm-find-files helm-multi-files
          helm-projectile-switch-project helm-projectile-find-file
          helm-gtags-find-pattern helm-gtags-find-tag-adapter
          helm-gtags-find-rtag-adapter helm-ag-select-directory
          find-function find-variable
          mark-defun mark-whole-buffer
          avy-goto-char avy-goto-char-2
          ensime-edit-definition
          ensime-edit-definition-with-fallback
          isearch-forward)))

(provide 'init-jumplist)
;;; init-jumplist.el ends here
