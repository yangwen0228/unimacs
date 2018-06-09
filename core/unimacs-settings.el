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
;; Global settings for built-in emacs parameters:
(setq
 undo-limit        1000000
 undo-strong-limit 1500000
 undo-outer-limit  12000000
 create-lockfiles          nil
 tooltip-delay             0.5
 case-fold-search          t
 compilation-scroll-output t
 ad-redefinition-action    'accept
 ;; no annoying beep on errors
 visible-bell t
 )

;; buffer local variables:
(setq-default
 display-line-numbers           t
 fill-column                    80
 grep-highlight-matches         t
 grep-scroll-output             t
 mouse-yank-at-point            t
 set-mark-command-repeat-pop    t
 indent-tabs-mode               nil
 tab-width                      4
 truncate-lines                 nil
 truncate-partial-width-windows nil
 )

(transient-mark-mode t)                       ; If you change buffer, or focus, disable the current buffer's mark
(setq ring-bell-function (lambda ()))         ; no annoying ring bell
(setq line-move-visual t)                     ; move around lines based on how they are displayed, rather than the actual line.
(setq next-line-add-newlines nil)             ; NO automatic new line when scrolling down at buffer bottom
(setq select-enable-clipboard t)              ; use system clipboard

;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-defun  'disabled nil)
;; Ctrl-x Ctrl-u/l to upper/lowercase regions without confirm
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ENCODING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'file-coding-system-alist '("\\.bat\\'"  . gb2312))
(add-to-list 'file-coding-system-alist '("\\.html\\'" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.js\\'"   . utf-8))
(add-to-list 'file-coding-system-alist '("\\.scala\\'"   . utf-8))
;; (add-to-list 'file-coding-system-alist '("\\.php"  . utf-8))
;; (add-to-list 'file-coding-system-alist '("\\.tcl"  . utf-8))
;; (add-to-list 'file-coding-system-alist '("\\.el"   . utf-8))

(prefer-coding-system        'utf-8-unix)
(set-default-coding-systems  'utf-8-unix)
(set-terminal-coding-system  'utf-8-unix)
(set-keyboard-coding-system  'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)

;; Chinese filenames in shell commands.
(when (eq system-type 'windows-nt)
  (set-selection-coding-system 'gbk-dos)
  (set-clipboard-coding-system 'gbk-dos)
  (set-file-name-coding-system 'gbk-dos)
  (setq default-process-coding-system '(gbk-dos . gbk-dos))
  (set-default 'process-coding-system-alist
               '(
                 ("find"   utf-8 . utf-8) ; use msys64/usr/bin/find.exe
                 ("global" utf-8 . utf-8)
                 ("gtags"  utf-8 . utf-8)
                 ("ctags"  utf-8 . utf-8)
                 ("ag"     utf-8 . utf-8)
                 ("java"   utf-8 . utf-8)
                 ("javac"  utf-8 . utf-8)
                 ))
  )

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

(defun unimacs-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil t))

(add-hook 'emacs-lisp-mode-hook 'unimacs-remove-elc-on-save)
(add-hook 'kill-emacs-hook 'unimacs-recompile-user-files)

(provide 'unimacs-settings)
;;; unimacs-settings.el ends here
