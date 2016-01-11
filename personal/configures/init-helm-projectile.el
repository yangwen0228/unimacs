;;; init-helm-projectile.el --- projectile work with helm
;;; Commentary:
;; comments

;;; Code:
(use-package helm-projectile
  ;; :bind (())
  :config
  (use-package projectile
    :config
    (projectile-global-mode 1)
    (setq projectile-svn-command "svn list -R . | grep -v '$/")
    (setq projectile-git-submodule-command
          "git submodule --quiet foreach 'echo $path'")
    (setq projectile-completion-system 'helm)
    ;; The alien is faster, but not well support for Windows.
    ;; The native always works, but slower.
    ;; To force the use of external indexing in Windows:
    (setq projectile-indexing-method 'alien)
    ;;(setq projectile-indexing-method 'native)
    ;; (setq projectile-indexing-method nil)

    ;; Use cache for big project.
    (setq projectile-enable-caching t)

    (setq projectile-mode-line
          '(:eval (format " Pj[%s]" (projectile-project-name)))))
  (helm-projectile-on))

(provide 'init-helm-projectile)
;;; init-helm-projectile.el ends here