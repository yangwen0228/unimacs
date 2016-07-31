;;; init-irony-eldoc.el --- configure for irony eldoc
;;; Commentary:
;; use irony and eldoc to show the function definitions.

;;; Code:
(add-hook 'irony-mode-hook 'irony-eldoc)

(provide 'init-irony-eldoc)
;;; init-irony-eldoc.el ends here