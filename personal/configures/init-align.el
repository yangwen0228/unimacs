;;; init-align.el --- Summary
;;; Commentary:
;; Align functions and keybindings.

;;; Code:
;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; set a 10
;; set lst "1 2 3"
;; =>
;; set a   10
;; set lst "1 2 3"
(defun my-align-by-space (start end)
  "Align by space repeat."
  (interactive "r")
  (align-regexp start end "\\(\\s-+\\)"
                1 1 t)
  (indent-region start end))

;; set a $b
;; set lst $c
;; =>
;; set a   $b
;; set lst $c
(defun my-align-by-dollar (start end)
  "Align by space repeat."
  (interactive "r")
  (align-regexp start end "\\(\\s-*\\)\\$"
                -1 1 t)
  (indent-region start end))

;;emacs                11505 227          3.6       3.3         1.9         68.9          93.5        68
;; vi                   1087  289          11.1      6.9         2.9         71.7          96.2        9
;; wolfram-mathematica  2993  360          4         2.2         1.9         66.7          92.5        51
;; =>
;; emacs                11505  227   3.6  3.3  1.9  68.9  93.5   68
;; vi                    1087  289  11.1  6.9  2.9  71.7  96.2    9
;; wolfram-mathematica   2993  360   4    2.2  1.9  66.7  92.5   51
(defun my-align-by-decimal (start end)
  "Align a table of numbers on (optional) decimal points."
  (interactive "r")
  (align-regexp start end "\\(\\s-*\\)\\$?\\(\\s-+[0-9]+\\)\\.?"
                -2 1 t))

(provide 'init-align)
;;; init-align.el ends here