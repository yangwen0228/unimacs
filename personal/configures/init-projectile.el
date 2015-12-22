(require 'projectile)

(projectile-global-mode 1)
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
      '(:eval (format " Pj[%s]" (projectile-project-name))))

(provide 'init-projectile)