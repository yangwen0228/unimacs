;;; unimacs-modes.el --- Summary: init unimacs core modes.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Configure some build-in modes or must init modes.

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
(eval-and-compile
  (require 'use-package))
(setq use-package-always-ensure t)
(require 'bind-key)
(require 'diminish)

(use-package dash :init (require 'dash))
(use-package f    :init (require 'f))
(use-package ht   :init (require 'ht))
(use-package s    :init (require 's))

(use-package anzu
  :init (global-anzu-mode t)
  :config
  (setq query-replace-skip-read-only t)
  :diminish "")

(use-package autorevert
  ;; revert buffers automatically when underlying files are changed externally
  :commands auto-revert-mode
  :diminish auto-revert-mode
  :init
  (global-auto-revert-mode t)
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

(use-package browse-kill-ring
  :bind ("M-y" . browse-kill-ring))

(use-package edit-server
  ;; Chrome editor.
  :disabled t
  :if (and window-system)
  :init
  (defun server-ensure-safe-dir (dir) "Noop" t)
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

(use-package eldoc
  :init
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.2)
  (setq eldoc-echo-area-use-multiline-p t)
  :diminish (eldoc-mode))

(use-package font-lock+
  :init
  (global-font-lock-mode t) ; turn on syntax highlighting for all buffers
  )
(use-package help+)
(use-package help-fns+)
(use-package help-mode+)

(use-package hippie-exp
  :disabled t
  :bind ("M-/" . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           ;; try-complete-file-name-partially
                                           ;; try-complete-file-name
                                           try-expand-all-abbrevs
                                           ;; try-expand-list
                                           try-expand-line
                                           ;; try-complete-lisp-symbol-partially
                                           ;; try-complete-lisp-symbol
                                           )))
(use-package mic-paren
  ;; highlight contents between braces.
  :init
  (paren-activate)
  (setq paren-match-face 'highlight)
  (setq paren-sexp-mode t)
  (eval-and-compile
    (require 'multiple-cursors))
  (defadvice mic-paren-highlight (around mic-paren-dont-highlight activate)
    (progn
      (mic-delete-overlay (aref mic-paren-overlays 0))
      (mic-delete-overlay (aref mic-paren-overlays 1))
      (mic-delete-overlay (aref mic-paren-overlays 2)))
    (unless (or mark-active multiple-cursors-mode)
      ad-do-it
      ))

  :diminish "")

(use-package midnight
  ;; midnight mode purges buffers which haven't been displayed in 3 days
  :init (midnight-mode t))

(use-package move-text
  :bind (("M-<up>"   . move-text-up)
         ("M-<down>" . move-text-down))
  :config
  (defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))
  :diminish "")

(use-package nlinum
  :preface
  (defun goto-line-with-feedback ()
    "Show line numbers temporarily, while prompting for the line number input"
    (interactive)
    (unwind-protect
        (progn
          (nlinum-mode 1)
          (let ((num (read-number "Goto line: ")))
            (goto-char (point-min))
            (forward-line (1- num))))
      (nlinum-mode -1)))
  :init
  (bind-key "C-c g" #'goto-line)
  (global-set-key [remap goto-line] 'goto-line-with-feedback))

(use-package page-break-lines
  :init (global-page-break-lines-mode)
  :diminish "")

(use-package server
  :ensure nil
  :config
  (when (and (eq window-system 'w32) (file-exists-p (getenv "APPDATA")))
    (setq server-auth-dir (concat (getenv "APPDATA") "/.emacs.d/server"))
    (unless (file-exists-p server-auth-dir)
      (make-directory server-auth-dir)))
  (defun server-ensure-safe-dir (dir) "Noop" t)
  (server-start))

(use-package subword
  ;; M-f better jump between camel words. C-M-f whole word.
  :init (add-hook 'prog-mode-hook 'subword-mode)
  :diminish (subword-mode))

(use-package tramp
  :config
  (with-eval-after-load 'tramp-cache
    (setq tramp-persistency-file-name
          (expand-file-name "tramp" unimacs-tempfiles-dir)))
  (setq tramp-default-user-alist '(("\\`su\\(do\\)?\\'" nil "root"))
        tramp-adb-program "/Users/yangwen/android-sdk-macosx/platform-tools/adb"
        ;; use the settings in ~/.ssh/config instead of Tramp's
        tramp-use-ssh-controlmaster-options nil
        backup-enable-predicate
        (lambda (name)
          (and (normal-backup-enable-predicate name)
               (not (let ((method (file-remote-p name 'method)))
                      (when (stringp method)
                        (member method '("su" "sudo"))))))))
  (setq tramp-default-method "ssh"
        tramp-auto-save-directory
        (expand-file-name "tramp" unimacs-tempfiles-dir)
        tramp-chunksize 8192)
  ;; (use-package tramp-sh
  ;;   :config
  ;;   (add-to-list 'tramp-remote-path "/usr/local/sbin")
  ;;   (add-to-list 'tramp-remote-path "/opt/java/current/bin")
  ;;   (add-to-list 'tramp-remote-path "~/bin"))
  )

(use-package visual-line-mode
  ;; break line to fit view
  :ensure nil
  :init
  (global-visual-line-mode t)
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)))

(use-package volatile-highlights
  ;; highlight when undo or yank
  :init
  (require 'volatile-highlights) ;Don't know why? Not work without this.
  (volatile-highlights-mode t)
  :diminish "")

(use-package whitespace
  ;; display special chars, like tabs and trailing whitespace.
  :disabled
  :init
  (global-whitespace-mode)
  :config
  ;; For some reason, having these in settings.el gets ignored if whitespace
  ;; loads lazily.
  (setq whitespace-auto-cleanup t
        whitespace-line-column 80
        whitespace-rescan-timer-time nil
        whitespace-silent t
        ;;tabs newline space-mark trailing tab-mark newline-mark face lines-tail
        whitespace-style '(face trailing tabs tab-mark))
  (setq whitespace-display-mappings
        '((space-mark nil); 32 SPACE, 183 MIDDLE DOT
          (newline-mark nil);(newline-mark 10 [172 10]) ; 10 LINE FEED
          (tab-mark 9 [8680 9] [92 9]); 9 TAB
          ))
  (setq whitespace-global-modes '(not org-mode eshell-mode shell-mode
                                      ;; emacs-lisp-mode clojure-mode lisp-mode
                                      web-mode log4j-mode "Web" dired-mode))
  ;; Just setq does not work! So must add a hook.
  (add-hook 'find-file-hook (lambda () (setq show-trailing-whitespace t)))
  :diminish (global-whitespace-mode whitespace-mode whitespace-newline-mode))

(use-package whole-line-or-region
  ;; kill or yank a whole line.
  :bind (("C-w" . whole-line-or-region-kill-region)
         ("M-w" . whole-line-or-region-kill-ring-save)
         ("C-y" . whole-line-or-region-yank))
  :diminish "")


(provide 'unimacs-modes)
;;; unimacs-modes.el ends here
