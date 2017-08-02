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
         ("C-:"           . mc/mark-all-like-this-in-defun-dwim)
         :map mc/keymap
         ("C-|" . mc/vertical-align-with-space)
         ("C-_" . undo)                 ;undo-tree-undo point position wrong.
         ("M-n" . mc/cycle-forward)
         ("M-p" . mc/cycle-backward))
  :preface (unbind-key "C-S-<mouse-1>")
  :init
  (require 'multiple-cursors)
  (setq mc/list-file (expand-file-name "mc-config.el" unimacs-personal-postload-dir))
  (setq mc/insert-numbers-default 1
        mc/cycle-looping-behaviour 'stop)

  (defun mc/my-quit ()
    "Quit from mark mode."
    (interactive)
    (mc/keyboard-quit)
    (multiple-cursors-mode 0))

  (defun mc/mark-all-symbols-like-this-toggle ()
    "Toogle when only one matches!"
    (interactive)
    (if (or multiple-cursors-mode (region-active-p))
        (mc/my-quit)
      (mc/mark-all-symbols-like-this)))

  (defun mc/mark-all-like-this-dwim ()
    "Toggle when not using region. When using region, search first,
if only one candidate searched, then quit!"
    (interactive)
    (if multiple-cursors-mode
        (mc/my-quit)
      (if (not (region-active-p))
          (mc/mark-all-symbols-like-this)
        (mc/mark-all-like-this)
        (unless multiple-cursors-mode
          (mc/my-quit)))))

  (defun mc/mark-all-like-this-in-defun-dwim ()
    "Like `mc/mark-all-like-this-dwim', but only in defun."
    (interactive)
    (if multiple-cursors-mode
        (mc/my-quit)
      (if (not (region-active-p))
          (mc/mark-all-symbols-like-this-in-defun)
        (mc/mark-all-like-this-in-defun)
        (unless multiple-cursors-mode
          (mc/my-quit)))))

  ;; mc and highlight-symbol-nav-mode key bindings conflict:
  (add-hook 'multiple-cursors-mode-enabled-hook
            (lambda () (when (boundp highlight-symbol-nav-mode)
                         (highlight-symbol-nav-mode -1))))
  (add-hook 'multiple-cursors-mode-disabled-hook
            (lambda () (when (boundp highlight-symbol-nav-mode)
                         (highlight-symbol-nav-mode +1)))))

(provide 'init-multiple-cursors)
;;; init-multiple-cursors.el ends here
