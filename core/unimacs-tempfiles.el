;;; unimacs-tempfiles.el --- Summary
;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file manages most of the tempfiles.

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

;; store all backup and autosave files in the tmp dir
(setq make-backup-files nil)
;; delete old backups silently
(setq delete-old-versions t)
;; (setq backup-directory-alist         `((".*" . ,temporary-file-directory)))

(use-package simple :ensure nil
  :init
  (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  ;; delete-auto-save-files
  (setq delete-auto-save-files t))

(use-package saveplace
  ;; saveplace remembers your location in a file when saving files
  :init
  (setq save-place-file (expand-file-name "saveplace" unimacs-tempfiles-dir))
  (save-place-mode t))

(use-package savehist
  ;; savehist keeps track of some history
  :init
  (setq savehist-file (expand-file-name "savehist" unimacs-tempfiles-dir)
        savehist-additional-variables '(search-ring regexp-search-ring)
        savehist-autosave-interval    60 ; save every minute
        history-length                100)
  (put 'minibuffer-history 'history-length 50)
  (put 'evil-ex-history    'history-length 50)
  (savehist-mode t))

(use-package recentf
  ;; save recent files
  :preface
  (defun unimacs-recentf-exclude-p (file)
    "A predicate to decide whether to exclude FILE from recentf."
    (let ((file-dir (file-truename (file-name-directory file))))
      (-any-p (lambda (dir)
                (string-prefix-p dir file-dir))
              (mapcar 'file-truename (list unimacs-tempfiles-dir
                                           package-user-dir
                                           "/tmp/")))))
  (defun unimacs-kill-buffer-and-remove-recentf ()
    "Kill the curent buffer and remove it from the recentf."
    (interactive)
    ;; (ido-buffer-internal 'kill 'kill-buffer "Kill buffer: "
    ;;                      (buffer-name (current-buffer)) nil 'ignore)
    (kill-buffer (current-buffer))
    (setq recentf-list (cdr recentf-list)))
  :init
  (bind-key (kbd "C-x M-k") 'unimacs-kill-buffer-and-remove-recentf)

  (setq recentf-save-file (expand-file-name "recentf" unimacs-tempfiles-dir)
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-max-saved-items 100
        recentf-max-menu-items  15
        recentf-auto-cleanup    'never
        recentf-exclude         '(unimacs-recentf-exclude-p)
        recentf-keep            '(file-remote-p file-readable-p))

  (recentf-mode t))

(use-package bookmark
  :init
  (setq bookmark-default-file (expand-file-name "bookmarks" unimacs-tempfiles-dir)
        bookmark-save-flag    1))

(use-package projectile
  ;; projectile is a project management mode
  :init
  (setq projectile-cache-file (expand-file-name  "projectile.cache" unimacs-tempfiles-dir)))

(use-package eshell
  :defer t
  :init
  (setq eshell-directory-name (expand-file-name "eshell" unimacs-tempfiles-dir)))

(use-package semantic
  :disabled t
  :defer t
  :init
  (setq semanticdb-default-save-directory
        (expand-file-name "semanticdb" unimacs-tempfiles-dir)))

(provide 'unimacs-tempfiles)
;;; unimacs-tempfiles.el ends here