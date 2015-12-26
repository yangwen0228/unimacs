;;; init-diminish.el --- Summary
;;; Commentary:
;; Diminish the mode names in mode line.

;;; Code:
(require 'diminish)
(dolist (mode '(
                yas-minor-mode
                auto-highlight-symbol-mode
                page-break-lines-mode
                hs-minor-mode
                smartparens-mode
                eldoc-mode
                whitespace-mode
                abbrev-mode
                ))
  ;; (add-hook (intern (concat (symbol-name mode) "-hook")) `(lambda () (diminish ',mode)))
  ;; another way to do this, but some times doesn't work.
  (eval-after-load mode (diminish mode))
  )

;; helm-gtags-mode is different
(add-hook 'helm-gtags-mode-hook '(lambda () (diminish 'helm-gtags-mode)))
(add-hook 'smartparens-mode-hook '(lambda () (diminish 'smartparens-mode)))

(provide 'init-diminish)
;;; init-diminish.el ends here
