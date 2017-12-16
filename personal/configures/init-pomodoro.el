;;; init-pomodoro.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package pomodoro
  :config
  (setq pomodoro-play-sounds nil)
  (pomodoro-add-to-mode-line))

(provide 'init-pomodoro)
;;; init-pomodoro.el ends here