;;; unimacs-settings.el --- Emacs Unimacs: emacs setting options.
;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Set the emacs default setting options.

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
(setq-default
 tab-width 4
 case-fold-search t
 compilation-scroll-output t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 grep-highlight-matches t
 grep-scroll-output t
 indent-tabs-mode nil
 mouse-yank-at-point t
 set-mark-command-repeat-pop t
 tooltip-delay 0.5
 fill-column 80
 truncate-lines nil
 truncate-partial-width-windows nil
 ;; no annoying beep on errors
 visible-bell t)

(transient-mark-mode t) ; If you change buffer, or focus, disable the current buffer's mark
(setq ring-bell-function (lambda ()))
;; move around lines based on how they are displayed, rather than the actual line.
(setq line-move-visual t)

;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-defun  'disabled nil)
                                        ;Ctrl-x Ctrl-u/l to upper/lowercase regions without confirm
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)

;; NO automatic new line when scrolling down at buffer bottom
(setq next-line-add-newlines nil)

(setq select-enable-clipboard t) ; use system clipboard

;; (setq buffer-file-coding-system 'cp936-dos)
;; (prefer-coding-system 'cp936-dos))

(setq buffer-file-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
;; Chinese in windows system.
;; Chinese filenames to Emacs and Emacsclientw:
;; @see: http://www.gnu.org/software/emacs/manual/html_node/emacs/File-Name-Coding.html
(when *win32* (setq file-name-coding-system 'gbk-dos))
;; Chinese filenames to apps:
(when (eq system-type 'windows-nt)
  (set-default 'process-coding-system-alist
               '(("find"   gbk-dos . gbk-dos)
                 ("global" gbk-dos . gbk-dos)
                 ("gtags"  gbk-dos . gbk-dos)
                 ("ctags"  gbk-dos . gbk-dos)
                 ("ag"     gbk-dos . gbk-dos)
                 )))

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defun unimacs-auto-save ()
  "Save the current buffer."
  (when (and buffer-file-name
             (buffer-modified-p (current-buffer))
             (file-writable-p buffer-file-name))
    (save-buffer)))

(defmacro unimacs-defadvice-commands (advice-name class commands &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.

The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command (,class ,(intern (concat (symbol-name command) "-" advice-name)) activate)
                    ,@body))
               commands)))

;; advise all window switching functions
(unimacs-defadvice-commands "auto-save"
                            before
                            (switch-to-buffer
                             other-window
                             select-window-by-number)
                            (unimacs-auto-save))

(add-hook 'mouse-leave-buffer-hook 'unimacs-auto-save)
(add-hook 'focus-out-hook          'unimacs-auto-save)

(provide 'unimacs-settings)
;;; unimacs-settings.el ends here
