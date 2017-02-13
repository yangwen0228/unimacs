;;; init-keyfreq.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package keyfreq
  ;; use keyfreq-show to see how many times you used a command.
  :init
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(provide 'init-keyfreq)
;;; init-keyfreq.el ends here