;;; init-resize-window.el --- Summary
;;; Commentary:
;; Use f b n p to resize windows.

;;; Code:
(use-package resize-window
  :commands (resize-window)
  :config
  (setq resize-window-dispatch-alist
        (append
         resize-window-dispatch-alist
         '((? resize-window--enlarge-down          " Resize - Expand down" t)
           (? resize-window--enlarge-up            " Resize - Expand up" t)
           (? resize-window--enlarge-horizontally  " Resize - horizontally" t)
           (? resize-window--shrink-horizontally   " Resize - shrink horizontally" t))))
  (setq resize-window-alias-list
        '((right ?f)
          (up ?p)
          (left ?b)
          (down ?n)))
  ;; overridden: the origin one doesn't take care of left/right window.
  (defun resize-window--enlarge-horizontally (&optional size)
    "Enlarge the window horizontally by one or optional SIZE."
    (let ((size (or size (resize-window-lowercase-argument))))
      (enlarge-window-horizontally
       (if (zerop (car (window-edges))) size (- size)))))

  (defun resize-window--shrink-horizontally (&optional size)
    "Shrink the window horizontally by one or optional SIZE."
    (let ((size (or size (resize-window-lowercase-argument))))
      (shrink-window-horizontally
       (if (zerop (car (window-edges))) size (- size)))))

  (defun resize-window--enlarge-down (&optional size)
    "Extend the current window downwards by optional SIZE.
If no SIZE is given, extend by `resize-window-default-argument`"
    (let ((size (or size (resize-window-lowercase-argument))))
      (enlarge-window
       (if (zerop (cadr (window-edges))) size (- size)))))

  (defun resize-window--enlarge-up (&optional size)
    "Bring bottom edge back up by one or optional SIZE."
    (let ((size (or size (resize-window-lowercase-argument))))
      (shrink-window
       (if (zerop (cadr (window-edges))) size (- size)))))

  (defun resize-window--display-choice (choice)
    "Formats screen message about CHOICE.
CHOICE is a \(key function description allows-capital\)."
    (let ((key (resize-window--choice-keybinding choice)))
      (format "%s: %s " (if (and (resize-window--allows-capitals choice)
                                 (> key 32))
                            (format "%s|%s"
                                    (string key)
                                    (string (- key 32)))
                          (string key))
              (resize-window--choice-documentation choice)))))

(provide 'init-resize-window)
;;; init-resize-window.el ends here