;;; init-workgroups2.el --- Summary
;;; Commentary:
;; comments

;;; Code:

(require 'workgroups2)
;; Your settings here

;;(setq wg-session-load-on-start t)    ; default: (not (daemonp))

;; Change prefix key (before activating WG)
(setq wg-prefix-key (kbd "C-c w"))

;; Change workgroups session file
(setq wg-session-file (expand-file-name ".emacs_workgroups" unimacs-tempfiles-dir))

;; Set your own keyboard shortcuts to reload/save/switch WGs:
;; "s" == "Super" or "Win"-key, "S" == Shift, "C" == Control
(global-set-key (kbd "C-c w r")     'wg-reload-session)
(global-set-key (kbd "C-S-<pause>") 'wg-save-session)
(global-set-key (kbd "s-z")         'wg-switch-to-workgroup)
(global-set-key (kbd "s-/")         'wg-switch-to-previous-workgroup)

(workgroups-mode 1)   ; put this one at the bottom of .emacs

(provide 'init-workgroups2)
;;; init-workgroups2.el ends here