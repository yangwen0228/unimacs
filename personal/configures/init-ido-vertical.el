;;; init-ido-vertical.el --- Makes ido-mode display prospects vertically
;;; Commentary:
;; Makes ido-mode display prospects vertically

;;; Code:
(require 'ido-vertical-mode)
(ido-vertical-mode 1)

(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(setq ido-vertical-show-count t)

;; colors 1
;; (setq ido-use-faces t)
;; (set-face-attribute 'ido-vertical-first-match-face nil
;;                     :background "#e5b7c0")
;; (set-face-attribute 'ido-vertical-only-match-face nil
;;                     :background "#e52b50"
;;                     :foreground "white")
;; (set-face-attribute 'ido-vertical-match-face nil
;;                     :foreground "#b00000")

;; colors 2
(setq ido-use-faces t)
(set-face-attribute 'ido-vertical-first-match-face nil
                    :background nil
                    :foreground "orange")
(set-face-attribute 'ido-vertical-only-match-face nil
                    :background nil
                    :foreground nil)
(set-face-attribute 'ido-vertical-match-face nil
                    :foreground nil)

(provide 'init-ido-vertical)
;;; init-ido-vertical.el ends here