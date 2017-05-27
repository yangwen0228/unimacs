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
(when (functionp 'menu-bar-mode)       (menu-bar-mode -1))
(when (functionp 'set-scroll-bar-mode) (set-scroll-bar-mode 'nil))
;; (when (functionp 'mouse-wheel-mode)    (mouse-wheel-mode -1))
(when (functionp 'tooltip-mode)        (tooltip-mode -1))
(when (functionp 'tool-bar-mode)       (tool-bar-mode -1))
(when (functionp 'blink-cursor-mode)   (blink-cursor-mode -1))

;; enable/disable startup screen: nil -> enable, t -> disable
(setq inhibit-startup-screen t)

;; nice scrolling
(setq smooth-scroll-margin 0
      scroll-margin 0
      scroll-conservatively 1000
      scroll-preserve-screen-position t)

;; mnemonic for utf-8 is "U", which is defined in the mule.el
(setq eol-mnemonic-mac  ":Mac")
(setq eol-mnemonic-unix ":Unix")
(setq eol-mnemonic-dos  ":Dos")
(setq eol-mnemonic-undecided ":?")

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

;; font settings:
(defvar unimacs-font-size 11)
(defun unimacs-make-font-string (font-name font-size)
  (if (and (stringp font-size)
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s %s" font-name font-size)))

(defun unimacs-set-font (frame font-size)
  "english-font-size could be set to \":pixelsize=18\" or a integer.
If set/leave chinese-font-size to nil, it will follow english-font-size"

  (require 'cl) ; for find if
  (let* ((english-fonts '("Consolas" "DejaVu Sans Mono" "Monospace" "Courier New"))
         (chinese-fonts '("新宋体" "黑体" "文泉驿等宽微米黑"))
         (en-font (unimacs-make-font-string (find-if #'x-list-fonts english-fonts)
                                          font-size))
         (zh-font (font-spec :family (find-if #'x-list-fonts chinese-fonts)
                             :size nil)))
    ;; Set the default English font
    (set-face-attribute 'default frame :font en-font)

    ;; Set Chinese font
    ;; Do not use 'unicode charset, it will cause the English font setting invalid
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset zh-font)))
  (message "default font size is now %d" font-size))

(unimacs-set-font nil unimacs-font-size)

(defun increase-default-font-height ()
  (interactive)
  (setq unimacs-font-size (1+ unimacs-font-size))
  (unimacs-set-font nil unimacs-font-size))

(defun decrease-default-font-height ()
  (interactive)
  (setq unimacs-font-size (1- unimacs-font-size))
  (unimacs-set-font nil unimacs-font-size))

(bind-key "C-M-=" 'increase-default-font-height)
(bind-key "C-M--" 'decrease-default-font-height)
(bind-key "M-+"   'text-scale-increase)   ; Font size +
(bind-key "M-_"   'text-scale-decrease)   ; Font size -

(use-package maxframe
  ;; NOTICE: Must put after font-size setup!
  :init
  (maximize-frame))

;; set the default theme
(use-package solarized-theme
  :init
  (load-theme 'solarized-dark t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "RoyalBlue2" :foreground "#002b36" :inverse-video t))))
 '(mc/cursor-face ((t (:background "black" :foreground "light blue" :inverse-video t)))))

(provide 'unimacs-ui)
;;; unimacs-ui.el ends here
