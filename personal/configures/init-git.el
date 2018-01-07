;;; init-git.el --- Summary
;;; Commentary:
;; comments

;;; Code:

(use-package magit
  :commands magit-mode
  :config
  (use-package ido-completing-read+)
  (setq magit-save-some-buffers nil
        magit-process-popup-time 10
        magit-completing-read-function 'magit-ido-completing-read)

  (defun magit-status-somedir ()
    (interactive)
    (let ((current-prefix-arg t))
      (magit-status default-directory)))

  ;; Sometimes I want check other developer's commit
  ;; show file of specific version
  (autoload 'magit-show "magit" "" t nil)
  ;; show the commit
  (autoload 'magit-show-commit "magit" "" t nil)

  (global-set-key [(meta f12)] 'magit-status)
  (global-set-key [(shift meta f12)] 'magit-status-somedir)

  (eval-after-load 'magit
    '(progn
       ;; Don't let magit-status mess up window configurations
       ;; http://whattheemacsd.com/setup-magit.el-01.html
       (defadvice magit-status (around magit-fullscreen activate)
         (window-configuration-to-register :magit-fullscreen)
         ad-do-it
         (delete-other-windows))

       (defun magit-quit-session ()
         "Restores the previous window configuration and kills the magit buffer"
         (interactive)
         (kill-buffer)
         (jump-to-register :magit-fullscreen))

       (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)))

  (when *is-a-mac*
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)]))))

  (eval-after-load 'magit
    '(progn
       (use-package magit-svn)
       ))

  ;;----------------------------------------------------------------------------
  ;; git-svn conveniences
  ;;----------------------------------------------------------------------------
  (eval-after-load 'compile
    '(progn
       (dolist (defn (list '(git-svn-updated "^\t[A-Z]\t\\(.*\\)$" 1 nil nil 0 1)
                           '(git-svn-needs-update "^\\(.*\\): needs update$" 1 nil nil 2 1)))
         (add-to-list 'compilation-error-regexp-alist-alist defn))
       (dolist (defn '(git-svn-updated git-svn-needs-update))
         (add-to-list 'compilation-error-regexp-alist defn))))

  (defvar git-svn--available-commands nil "Cached list of git svn subcommands")

  (defun git-svn (dir)
    "Run git svn"
    (interactive "DSelect directory: ")
    (unless git-svn--available-commands
      (setq git-svn--available-commands
            (string-all-matches "^  \\([a-z\\-]+\\) +" (shell-command-to-string "git svn help") 1)))
    (let* ((default-directory (vc-git-root dir))
           (compilation-buffer-name-function (lambda (major-mode-name) "*git-svn*")))
      (compile (concat "git svn "
                       (ido-completing-read "git-svn command: " git-svn--available-commands nil t)))))

  (defun git-reset-current-file ()
    "git reset file of current buffer"
    (interactive)
    (let ((filename))
      (when buffer-file-name
        (setq filename (file-truename buffer-file-name))
        (shell-command (concat "git reset " filename))
        (message "DONE! git reset %s" filename)
        )))

  (defun git-add-current-file ()
    "git add file of current buffer"
    (interactive)
    (let ((filename))
      (when buffer-file-name
        (setq filename (file-truename buffer-file-name))
        (shell-command (concat "git add " filename))
        (message "DONE! git add %s" filename)
        )))

  (defun git-push-remote-origin ()
    "run `git push'"
    (interactive)
    (when buffer-file-name
      (message "(pwd)=%s" default-directory)
      (shell-command (concat "cd " (pwd) ";git push"))
      (message "DONE! git push at %s" default-directory)
      ))

  (defun git-add-option-update ()
    "git add only tracked files of default directory"
    (interactive)
    (when buffer-file-name
      (shell-command "git add -u")
      (message "DONE! git add -u %s" default-directory)
      ))

  ;; turn off the overlay, I do NOT want to lose original syntax highlight!
  (setq magit-highlight-overlay t)
  ;; }}
  )
;; require curl
(use-package gitter :disabled
  :commands gitter
  :config
  (setq gitter-token "c1024ce4d68a1b8a4780b6a104ffcbd3895ff976"))

(provide 'init-git)
;;; init-git.el ends here
