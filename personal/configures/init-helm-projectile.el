;;; init-helm-projectile.el --- projectile work with helm
;;; Commentary:
;; comments

;;; Code:
(use-package helm-projectile
  :defer t
  :config
  (require 'helm-projectile)
  (helm-projectile-on)
  )

(provide 'init-helm-projectile)
;;; init-helm-projectile.el ends here