;;; init-chm-help.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package chm-help
  :ensure nil
  :bind ("C-c h" . chm-help-lookup)
  :config
  (setq chm-help-lookup-alist
        '(("tcl-mode" . "D:/softwares/work/tcl/apps/twapi/TclWinHelp-2015-05-01.chm")
          ("java-mode" . "e:/ebooks/Java Platform SE 8.chm"))))

(provide 'init-chm-help)
;;; init-chm-help.el ends here