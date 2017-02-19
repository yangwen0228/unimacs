;;; init-edit-server.el --- Summary
;;; Commentary:
;; Used to edit Chrome pages.

;;; Code:
(use-package edit-server :disabled t
  ;; Eidt with Emacs: Chrome plugin and editor. Not Emacs deamon or server.
  :if (and window-system)
  :init
  (defun server-ensure-safe-dir (dir) "Noop" t)
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

(provide 'init-edit-server)
;;; init-edit-server.el ends here