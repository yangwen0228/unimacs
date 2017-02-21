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
(require 'use-package)
(setq use-package-always-ensure t)

(use-package anzu
  :defer 0
  :config
  (global-anzu-mode t)
  (setq query-replace-skip-read-only t)
  :diminish "")

(use-package autorevert
  ;; revert buffers automatically when underlying files are changed externally
  :commands auto-revert-mode
  :diminish auto-revert-mode
  :init
  (add-hook 'find-file-hook #'(lambda () (auto-revert-mode 1)))
  ;; :config (setq global-auto-revert-non-file-buffers t auto-revert-verbose nil)
  )

(use-package browse-kill-ring
  :bind ("M-y" . browse-kill-ring))

(use-package ediff :ensure nil
  :commands (ediff-buffers
             ediff-buffers3 compare-windows
             ediff-files ediff-files ediff-files3
             ediff-revision ediff-patch-file ediff-patch-buffer
             ediff-regions-linewise ediff-regions-wordwise)
  :init
  (defun my-kill-ediff-buffers ()
    (kill-buffer ediff-buffer-A)
    ;; (kill-buffer ediff-buffer-B)
    (kill-buffer ediff-buffer-C))

  (add-hook 'ediff-quit-hook 'my-kill-ediff-buffers)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo))

(use-package eldoc :ensure nil
  :init
  (setq eldoc-idle-delay 0.2)
  (setq eldoc-echo-area-use-multiline-p t)
  (eldoc-mode t)
  :diminish (eldoc-mode))

(use-package font-lock+
  :init
  (global-font-lock-mode t) ; turn on syntax highlighting for all buffers
  )
(use-package help+)
(use-package help-fns+)
(use-package help-mode+)

(use-package hippie-exp :disabled t
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
  :defer 0
  :config
  (paren-activate)
  (setq paren-match-face 'highlight)
  (setq paren-sexp-mode t)
  ;; Fix: Error with multiple-cursors.
  (defadvice mic-paren-highlight (after mic-paren-highlight activate)
    (if (or mark-active
            (and (boundp 'multiple-cursors-mode)
                 multiple-cursors-mode))
        (progn
          (mic-delete-overlay (aref mic-paren-overlays 0))
          (mic-delete-overlay (aref mic-paren-overlays 1))
          (mic-delete-overlay (aref mic-paren-overlays 2)))))

  :diminish "")

(use-package midnight :disabled :ensure nil
  ;; midnight mode purges buffers every midnight-period time.
  :config
  (midnight-mode t)
  (midnight-delay-set 'midnight-delay "12:00am")  ; delay to midnight 24:00.
  (setq clean-buffer-list-delay-general 0)       ; today's buffers.
  (setq midnight-period 3600)                    ; run / 1 hour

  (add-to-list 'clean-buffer-list-kill-buffer-names "*vc-dir*")
  (add-to-list 'clean-buffer-list-kill-regexps "\\*helm")
  (add-to-list 'clean-buffer-list-kill-never-buffer-names "*helm ag* *helm gtags* *helm swoop*"))

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
  ;; When file is big, linum-mode is very slow.
  ;; So just display line number when goto line.
  :bind (("M-g g" . goto-line)
         ("M-g l" . goto-line))
  :init
  (global-set-key [remap goto-line] 'goto-line-with-feedback)
  (defun goto-line-with-feedback ()
    "Show line numbers temporarily, while prompting for the line number input"
    (interactive)
    (unwind-protect
        (progn
          (nlinum-mode 1)
          (let ((num (read-number "Goto line: ")))
            (goto-char (point-min))
            (forward-line (1- num))))
      (nlinum-mode -1))))

(use-package page-break-lines
  :defer 0
  :config (global-page-break-lines-mode)
  :diminish "")

(use-package scratch)

(use-package server :ensure nil
  ;; edit server, must use :config. cannot use :init.
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

(use-package tramp :disabled
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

(use-package simple :ensure nil
  ;; visual-line-mode: break line to fit view
  :defer 0
  :config
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
  ;; tab-mark lead to freeze Emacs!
  :init
  (global-whitespace-mode)
  (setq whitespace-line-column 120
        whitespace-style '(face trailing tabs))
  (setq whitespace-display-mappings
        '(
          ;; (space-mark nil); 32 SPACE, 183 MIDDLE DOT
          ;; (newline-mark nil);(newline-mark 10 [172 10]) ; 10 LINE FEED
          ;; (tab-mark 9 [8680 9] [92 9]); 9 TAB
          ))
  ;; (setq whitespace-global-modes '(not org-mode eshell-mode shell-mode
  ;;                                     ;; emacs-lisp-mode clojure-mode lisp-mode
  ;;                                     web-mode log4j-mode dired-mode))

  :diminish (global-whitespace-mode whitespace-mode whitespace-newline-mode))

(use-package winner
  :bind (("C-c ," . winner-undo)
         ("C-c ." . winner-redo)))

(use-package whole-line-or-region
  ;; kill or yank a whole line.
  :bind (("C-w" . whole-line-or-region-kill-region)
         ("M-w" . whole-line-or-region-kill-ring-save)
         ("C-y" . whole-line-or-region-yank))
  :diminish "")

(use-package benchmark-init
  :commands (benchmark-init/activate))

(provide 'unimacs-modes)
;;; unimacs-modes.el ends here
