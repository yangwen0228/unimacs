;;; init-eclim.el --- Summary
;;; Commentary:
;; Java development IDE.

;;; Code:

(use-package eclim
  :mode ("\\.java$" . java-mode)
  :config
  (require 'eclimd)
  (custom-set-variables
   '(eclim-eclipse-dirs '("C:/Program Files (x86)/eclipse"))
   '(eclim-executable "\"C:/Program Files (x86)/eclipse/eclim.bat\"")
   '(eclimd-executable "C:/Program Files (x86)/eclipse/eclimd.bat"))
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer))

(provide 'init-eclim)
;;; init-eclim.el ends here




