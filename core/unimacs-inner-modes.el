;;; unimacs-inner-modes.el --- Summary: init inner modes.

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
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
(require 'bind-key)
(require 'diminish)

(use-package anzu
  :init (global-anzu-mode t)
  :diminish "")

(use-package browse-kill-ring
  :bind ("M-y" . browse-kill-ring))

(use-package mic-paren
  ;; highlight contents between braces.
  :init
  (paren-activate)
  (setq paren-match-face 'highlight)
  (setq paren-sexp-mode t)
  :diminish "")

(use-package midnight
  ;; midnight mode purges buffers which haven't been displayed in 3 days
  :init (midnight-mode t))

(use-package move-text
  :bind (("M-<up>"   . move-text-up)
         ("M-<down>" . move-text-down))
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
  :init (global-page-break-lines-mode t)
  :diminish "")

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

(use-package volatile-highlights
  ;; highlight when undo or yank
  :init
  (require 'volatile-highlights) ;Don't know why? Not work without this.
  (volatile-highlights-mode t)
  :diminish "")

(use-package whitespace
  ;; display special chars, like tabs and trailing whitespace.
  :init
  (setq whitespace-line-column 80)
  (setq whitespace-style '(tabs newline space-mark
                                tab-mark newline-mark
                                face lines-tail))
  (setq whitespace-display-mappings
        ;; all numbers are Unicode codepoint in decimal. e.g. (insert-char 182 1)
        ;; 32 SPACE, 183 MIDDLE DOT
        '((space-mark nil)
          ;;(newline-mark 10 [172 10]) ; 10 LINE FEED
          (newline-mark nil)
          ;; (tab-mark 9 [183 9] [92 9]); 9 TAB, MIDDLE DOT
          (tab-mark 9 [8680 9] [92 9]); 9 TAB, MIDDLE DOT
          ))
  (setq whitespace-global-modes '(not org-mode
                                      eshell-mode
                                      shell-mode
                                      web-mode
                                      log4j-mode
                                      "Web"
                                      dired-mode
                                      ;; emacs-lisp-mode
                                      ;; clojure-mode
                                      ;; lisp-mode
                                      ))
  (global-whitespace-mode)
  (setq show-trailing-whitespace t)
  :diminish (global-whitespace-mode . ""))

(use-package whole-line-or-region
  ;; kill or yank a whole line.
  :bind (("C-w" . whole-line-or-region-kill-region)
         ("M-w" . whole-line-or-region-kill-ring-save)
         ("C-y" . whole-line-or-region-yank))
  :diminish "")


(provide 'unimacs-inner-modes)
;;; unimacs-inner-modes.el ends here