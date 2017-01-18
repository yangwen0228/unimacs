;;; init-java.el --- Summary
;;; Commentary:
;; Java development.

;;; Code:
(use-package eclim
  :mode ("\\.java$" . java-mode)
  :config
  (require 'eclimd)
  (setq eclimd-autostart t)
  (global-eclim-mode)
  (custom-set-variables
   '(eclim-eclipse-dirs '("C:/Program Files (x86)/eclipse"))
   '(eclim-executable "\"C:/Program Files (x86)/eclipse/eclim.bat\"")
   '(eclimd-executable "C:/Program Files (x86)/eclipse/eclimd.bat"))
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)

  (use-package company-emacs-eclim
    :init
    (company-emacs-eclim-setup)
    (unimacs-company-define-backends
     '((java-mode) . (company-emacs-eclim company-dabbrev-code))))
  )

(provide 'init-java)
;;; init-java.el ends here