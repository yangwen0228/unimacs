;;; init-java.el --- Summary
;;; Commentary:
;; Java development.

;;; Code:
(use-package eclim :disabled
  :mode ("\\.java$" . eclim-mode)
  :commands (start-eclimd)
  :config
  (require 'eclimd)
  (setq eclimd-autostart t)
  (global-eclim-mode)
  ;; (custom-set-variables
  ;;  '(eclim-eclipse-dirs '("C:/Program Files (x86)/eclipse"))
  ;;  '(eclim-executable "\"C:/Program Files (x86)/eclipse/eclim.bat\"")
  ;;  '(eclimd-executable "C:/Program Files (x86)/eclipse/eclimd.bat"))
  (custom-set-variables
   '(eclim-eclipse-dirs '("D:/portable/eclipse"))
   '(eclim-executable "\"D:/portable/eclipse/eclim.bat\"")
   '(eclimd-executable "D:/portable/eclipse/eclimd.bat"))
  (setq eclimd-wait-for-process nil
        eclimd-default-workspace "c:/Users/yangwen/workspace/"
        help-at-pt-display-when-idle t
        help-at-pt-timer-delay 0.1)

  (help-at-pt-set-timer)

  (use-package company-emacs-eclim
    :init
    (company-emacs-eclim-setup)
    (unimacs-company-define-backends
     '((java-mode) . (company-emacs-eclim company-dabbrev-code)))
    (add-to-list 'eclim--file-coding-system-mapping '("chinese-iso-8bit-dos" . "gb2312")))
  )

(use-package jdee
  :mode ("\\.java$" . jdee-mode)
  :config
  (setq jdee-server-dir (expand-file-name "jars" unimacs-utils-dir)
        jdee-complete-function 'jdee-complete-minibuf)
  ;; (bind-key "<tab>" 'helm-yas-complete jdee-mode-map)
  )

(use-package meghanada :disabled
  ;; Can't work on Windows yet!
  :mode ("\\.java$" . meghanada-mode)
  :config
  )

(provide 'init-java)
;;; init-java.el ends here