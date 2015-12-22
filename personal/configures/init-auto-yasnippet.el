;;; init-auto-yasnippet.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(require 'auto-yasnippet)
(global-set-key (kbd "C-c y c") #'aya-create)
(global-set-key (kbd "C-c y e") #'aya-expand)
(global-set-key (kbd "C-c y o") #'aya-open-line)

(provide 'init-auto-yasnippet)
;;; init-auto-yasnippet.el ends here
