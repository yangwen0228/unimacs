;;; init-chm-help.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package chm-help
  :bind ("C-c h" . chm-help-lookup)
  :ensure nil
  :config
  (setq chm-help-lookup-alist
        '(("tcl-mode" . "c:/Tcl8.6/doc/ActiveTclHelp8.6.chm")
          ("java-mode" . "e:/ebooks/JDK_API_1_6_zh_CN.CHM")))
  )

(provide 'init-chm-help)
;;; init-chm-help.el ends here