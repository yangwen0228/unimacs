;;; init-revive.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package revive
  :bind (:map ctl-x-map
              ("S" . save-current-configuration)
              ("F" . resume)
              ("K" . wipe)))

(provide 'init-revive)
;;; init-revive.el ends here