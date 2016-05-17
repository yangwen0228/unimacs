;;; init-align.el --- Summary
;;; Commentary:
;; Align functions and keybindings.
;; Align your code in a pretty way.

;;; Code:
(use-package align
  :bind (("C-x \\" . align-regexp)
         ("C-x |" . align-regexp-repeated))
  :preface
  (defun align-regexp-repeated (start stop regexp)
    "Like align-regexp, but repeated for multiple columns. See
http://www.emacswiki.org/emacs/AlignCommands"
    (interactive "r\nsAlign regexp: ")
    (let ((regexp (if (string= regexp "") " " regexp))
          (spacing 1)
          (old-buffer-size (buffer-size)))
      ;; If our align regexp is just spaces, then we don't need any
      ;; extra spacing.
      (when (string-match regexp " ")
        (setq spacing 0))
      (align-regexp start stop
                    ;; add space at beginning of regexp
                    (concat "\\([[:space:]]*\\)" regexp)
                    1 spacing t)
      ;; modify stop because align-regexp will add/remove characters
      (align-regexp start (+ stop (- (buffer-size) old-buffer-size))
                    ;; add space at end of regexp
                    (concat regexp "\\([[:space:]]*\\)")
                    1 0 t)
      (indent-region start (+ stop (- (buffer-size) old-buffer-size)))))
  ;; <item name="test" age ="20"/>
  ;; <item name="test2" age ="20"/>
  ;; =>
  ;; <item name="test"  age ="20"/>
  ;; <item name="test2" age ="20"/>
  (defun my-align-xml-by= (start end)
    "Align by space repeat."
    (interactive "r")
    (align-regexp start end "\\(\\s-+\\)[^ ]+\\s-*=" -1 1 t)
    (indent-region start end))

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
    "Align by $ sign repeat."
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
  :config
  )

(provide 'init-align)
;;; init-align.el ends here
