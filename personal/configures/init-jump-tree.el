;;; init-jump-tree.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package jump-tree
  :config
  (global-jump-tree-mode 1)
  (global-set-key (kbd "C-x j") 'jump-tree-visualize)
  ;; The keybind M-, and M-. conflict with other modes.
  ;; Use <left> and <right>, same as intellij.
  (global-set-key (kbd "C-M-<left>") 'jump-tree-jump-prev)
  (global-set-key (kbd "C-M-<right>") 'jump-tree-jump-next)
  (global-set-key (kbd "C-M-<up>") 'previous-buffer)
  (global-set-key (kbd "C-M-<down>") 'next-buffer)

  (setq jump-tree-pos-list-record-commands
        '(save-buffer
          beginning-of-buffer
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

(provide 'init-jump-tree)
;;; init-jump-tree.el ends here
