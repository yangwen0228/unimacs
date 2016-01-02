;;; init-fonts.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(require 'cl)

(let ((font))
  (cond
   (*win32*    (setq font "Consolas"))
   (*is-a-mac* (setq font "Consolas")))
  (set-face-attribute 'default nil
                      :family font
                      :height 110 :weight 'normal)
  (setq-default line-spacing 0.15))

(defun font-name-replace-size (font-name new-size)
  (let ((parts (split-string font-name "-")))
    (setcar (nthcdr 7 parts) (format "%d" new-size))
    (mapconcat 'identity parts "-")))

(defun increment-default-font-height (delta)
  "Adjust the default font height by DELTA on every frame.
The pixel size of the frame is kept (approximately) the same.
DELTA should be a multiple of 10, in the units used by the
:height face attribute."
  (let* ((new-height (+ (face-attribute 'default :height) delta))
         (new-point-height (/ new-height 10)))
    (dolist (f (frame-list))
      (with-selected-frame f
        ;; Latest 'set-frame-font supports a "frames" arg, but
        ;; we cater to Emacs 23 by looping instead.
        (set-frame-font (font-name-replace-size (face-font 'default)
                                                new-point-height)
                        nil)))
    (set-face-attribute 'default nil :height new-height)
    (message "default font size is now %d" new-point-height)))

(defun increase-default-font-height ()
  (interactive)
  (increment-default-font-height 10))

(defun decrease-default-font-height ()
  (interactive)
  (increment-default-font-height -10))

(bind-key "C-M-=" 'increase-default-font-height)
(bind-key "C-M--" 'decrease-default-font-height)
(bind-key "M-+"   'text-scale-increase)   ; Font size +
(bind-key "M-_"   'text-scale-decrease)   ; Font size -

(provide 'init-fonts)
;;; init-fonts.el ends here
