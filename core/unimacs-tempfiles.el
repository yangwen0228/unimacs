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
(setq backup-directory-alist         `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; autosave the undo-tree history
(setq undo-tree-history-directory-alist `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)

;; delete-auto-save-files
(setq delete-auto-save-files t)
;; delete old backups silently
(setq delete-old-versions t)

;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :init
  (setq save-place-file (expand-file-name "saveplace" unimacs-tempfiles-dir))
  (save-place-mode))

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" unimacs-tempfiles-dir))
(savehist-mode +1)

;; save recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" unimacs-tempfiles-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)

(defun unimacs-recentf-exclude-p (file)
  "A predicate to decide whether to exclude FILE from recentf."
  (let ((file-dir (file-truename (file-name-directory file))))
    (-any-p (lambda (dir)
              (string-prefix-p dir file-dir))
            (mapcar 'file-truename (list unimacs-tempfiles-dir package-user-dir)))))

(add-to-list 'recentf-exclude 'unimacs-recentf-exclude-p)

(recentf-mode +1)
;; bookmarks
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" unimacs-tempfiles-dir)
      bookmark-save-flag 1)

;; projectile is a project management mode
(require 'projectile)
(setq projectile-cache-file (expand-file-name  "projectile.cache" unimacs-tempfiles-dir))
(projectile-global-mode t)
(setq eshell-directory-name (expand-file-name "eshell" unimacs-tempfiles-dir))
(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" unimacs-tempfiles-dir))

(provide 'unimacs-tempfiles)
;;; unimacs-tempfiles.el ends here