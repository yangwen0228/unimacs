;;; init-flx.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package flx
  :config
  (use-package flx-ido
    :init
    ;; (ido-mode 1)
    ;; (ido-everywhere 1)
    (flx-ido-mode 1)
    (setq ido-enable-flex-matching t)
    ;; disable ido faces to see flx highlights.
    (setq ido-use-faces nil
          flx-ido-use-faces t)
    ))

(provide 'init-flx)
;;; init-flx.el ends here
