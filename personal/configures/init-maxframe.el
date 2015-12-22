(autoload 'mf-max-display-pixel-width "maxframe" "" nil)
(autoload 'mf-max-display-pixel-height "maxframe" "" nil)
(autoload 'maximize-frame "maxframe" "" t)
(autoload 'restore-frame "maxframe" "" t)

(when *is-cocoa-emacs*
  (eval-after-load 'maxframe
    '(progn
       (fset 'maximize-frame 'x-maximize-frame)
       (fset 'restore-frame 'x-restore-frame))))

(when *is-a-mac*
  (setq mf-display-padding-width 4
        mf-offset-x 0
        mf-offset-y 0
        mf-display-padding-height (if (when (boundp 'ns-auto-hide-menu-bar)
                                        ns-auto-hide-menu-bar)
                                      63
                                    (+ 27 63))))

(require 'init-funcs) ; for with-selected-frame

(defvar sanityinc/prev-frame nil "The selected frame before invoking `make-frame-command'.")
(defadvice make-frame-command (before sanityinc/note-previous-frame activate)
  "Record the selected frame before creating a new one interactively."
  (setq sanityinc/prev-frame (selected-frame)))

(defun maybe-maximize-frame (&optional frame)
  (with-selected-frame frame
    (when (and window-system
               sanityinc/prev-frame
               (maximized-p sanityinc/prev-frame))
      (maximize-frame))))

(add-hook 'after-make-frame-functions 'maybe-maximize-frame)
(add-hook 'after-init-hook 'maybe-maximize-frame)

(defun within-p (a b delta)
  (<= (abs (- b a)) delta))

(defun maximized-p (&optional frame)
  (or (not (with-selected-frame frame window-system))
      (and (within-p (mf-max-display-pixel-width)
                     (frame-pixel-width frame)
                     (frame-char-width frame))
           (within-p (mf-max-display-pixel-height)
                     (+ mf-display-padding-height (frame-pixel-height frame))
                     (frame-char-height frame)))))

;; max frame, @see https://github.com/rmm5t/maxframe.el
;; (Setq mf-max-width 1600) ;; Pixel width of main monitor. for dual-lcd only
(when *is-a-mac*
  (let ((x (x-display-pixel-width))
        (y (x-display-pixel-height)))
    (if (> (/ x y) 2)
        (setq mf-max-width 1280) ;; Pixel width of main monitor. for dual-lcd only
        )))

(add-hook 'window-setup-hook 'maximize-frame t)

(provide 'init-maxframe)
