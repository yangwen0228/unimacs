;;; init-number.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package number
  :preface
  (defun mc/add-numbers (arg)
    "Add numbers for each cursor, starting at
`mc/insert-numbers-default' or ARG."
    (interactive "P")
    (require 'number)
    (let* ((number (or (and arg (prefix-numeric-value arg)) 1))
           (command (lambda () (interactive)
                      (number-arith-op
                       (number-read (number-to-string number))
                       '+))))
      (call-interactively command)
      (mc/execute-command-for-all-fake-cursors command)))
  :commands mc/add-numbers number/add number/sub number/multiply number/divide number/eval)

(provide 'init-number)
;;; init-number.el ends here