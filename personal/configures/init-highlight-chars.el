;;; init-highlight-chars.el --- configure highlight a set of chars
;; 
;;; Commentary: 
;; 
;; 
;;; Code:
(require 'highlight-chars)
(add-hook 'font-lock-mode-hook 'hc-highlight-tabs)

(provide 'init-highlight-chars)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-highlight-chars.el ends here
