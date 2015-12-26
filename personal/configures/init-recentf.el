;; @see http://stackoverflow.com/questions/2068697/emacs-is-slow-opening-recent-files
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-max-saved-items 100
      recentf-exclude '("/tmp/"
                        "/ssh:"
                        "/sudo:"
                        "/home/[a-z]\+/\\."
                        ))

(recentf-mode 1)

(defun my-kill-buffer-remove-recentf ()
  "Kill the curent buffer and remove it from the recentf."
  (interactive)
  ;; (ido-buffer-internal 'kill 'kill-buffer "Kill buffer: "
  ;;                      (buffer-name (current-buffer)) nil 'ignore)
  (kill-buffer (current-buffer))
  (setq recentf-list (cdr recentf-list)))

(global-set-key (kbd "C-x M-k") 'my-kill-buffer-remove-recentf)

(provide 'init-recentf)
