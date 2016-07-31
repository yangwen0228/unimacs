;;; init-smart-mode-line.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(require 'smart-mode-line)
(if (require 'smart-mode-line nil 'noerror)
    (progn
      (setq sml/name-width 20)
      (setq sml/mode-width 'full)
      (setq sml/shorten-directory t)
      (setq sml/shorten-modes t)

      (rich-minority-mode 1)
      (setq rm-blacklist '(" GitGutter" " MRev" " company" " mate" " Projectile"))

      (if after-init-time
        (sml/setup)
        (add-hook 'after-init-hook 'sml/setup))

      (require 'smart-mode-line-powerline-theme)
      (sml/apply-theme 'powerline)
      ;; Alternatives:
      ;; (sml/apply-theme 'powerline)
      ;; (sml/apply-theme 'dark)
      ;; (sml/apply-theme 'light)
      ;; (sml/apply-theme 'respectful)
      ;; (sml/apply-theme 'automatic)

      (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/" ":DB:"))
      (add-to-list 'sml/replacer-regexp-list '("^~/Code/" ":CODE:"))
      (add-to-list 'sml/replacer-regexp-list '("^:CODE:investor-bridge" ":IB:"))
      (add-to-list 'sml/replacer-regexp-list '("^~/.*/lib/ruby/gems" ":GEMS" ))
      ))

(provide 'init-smart-mode-line)
;;; init-smart-mode-line.el ends here
