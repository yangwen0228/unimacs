;;; unimacs-ui.el --- Emacs Unimacs: UI optimizations and tweaks.
;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;; We dispense with most of the point and click UI, reduce the startup noise,
;; configure smooth scolling and a nice theme that's easy on the eyes (solarized dark).

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; set the default theme
(load-theme 'solarized-dark t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "RoyalBlue2" :foreground "#002b36" :inverse-video t)))))

(when (functionp 'menu-bar-mode)       (menu-bar-mode -1))
(when (functionp 'set-scroll-bar-mode) (set-scroll-bar-mode 'nil))
;; (when (functionp 'mouse-wheel-mode)    (mouse-wheel-mode -1))
(when (functionp 'tooltip-mode)        (tooltip-mode -1))
(when (functionp 'tool-bar-mode)       (tool-bar-mode -1))
(when (functionp 'blink-cursor-mode)   (blink-cursor-mode -1))

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq smooth-scroll-margin 0
      scroll-margin 0
      scroll-conservatively 1000
      scroll-preserve-screen-position t)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; make the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
(fringe-mode 8)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '(" Unimacs - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; font settings.
(let ((font)) (cond
   (*win32*    (setq font "Consolas"))
   (*is-a-mac* (setq font "Consolas")))
  (set-face-attribute 'default nil
                      :family font :height 110 :weight 'normal)
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

(provide 'unimacs-ui)
;;; unimacs-ui.el ends here
