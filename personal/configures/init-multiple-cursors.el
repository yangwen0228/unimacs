;;; init-multiple-cursors.el --- Summary
;;; Commentary:
;; IMPORTANT: multiple-cursors use ~/.emacs.d/.mc-lists.el to keep choose settings.
;; https://github.com/magnars/multiple-cursors.el

;;; Code:
(use-package multiple-cursors
  :bind (("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-M->"         . mc/skip-to-next-like-this)
         ("C-M-<"         . mc/skip-to-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         ("C-;"           . mc/mark-all-like-this-dwim)
         ("C-:"           . mc/mark-all-like-this-in-defun)
         :map mc/keymap
         ("C-|" . mc/vertical-align-with-space)
         ("C-_" . undo)                 ;undo-tree-undo point position wrong.
         ("C-;" . mc/my-quit)
         ("M-n" . mc/cycle-forward)
         ("M-p" . mc/cycle-backward)
         )
  :init
  ;; The bind commonds none require 'multiple-cursors.
  ;; Must use :init and require to let the mc/keymap init.
  (require 'multiple-cursors)
  (defun mc/my-quit ()
    "Quit from mark mode."
    (interactive)
    (mc/keyboard-quit)
    (multiple-cursors-mode 0))

  (setq mc/insert-numbers-default 1))

(provide 'init-multiple-cursors)
;;; init-multiple-cursors.el ends here
