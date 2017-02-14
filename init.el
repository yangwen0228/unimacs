;;; init.el --- Unimacs's configuration entry point.
;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file simply sets up the default load path and requires
;; the various modules defined within Emacs Unimacs.

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
(setq my-benchmark nil)
;; (setq max-lisp-eval-depth 5000)
(setq max-specpdl-size 5000)
;; (setq debug-on-error t)
(setq inhibit-default-init t) ; bug @ ido.el about 'seq

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold (* 40 1024 1024))

;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
(setq *macbook-pro-support-enabled* t)
(setq *is-a-mac*        (eq system-type 'darwin))
(setq *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac)))
(setq *is-cocoa-emacs*  (and *is-a-mac* (eq window-system 'ns)))
(setq *win32*           (eq system-type 'windows-nt) )
(setq *mingw32*         (eq system-type 'mingw32) )
(setq *cygwin*          (eq system-type 'cygwin) )
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *unix*  (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )
(setq *linux-x* (and window-system *linux*) )
(setq *xemacs*  (featurep 'xemacs) )
(setq *emacs23* (and (not *xemacs*) (or (>= emacs-major-version 23))) )
(setq *emacs24* (and (not *xemacs*) (or (>= emacs-major-version 24))) )

(defvar current-user (getenv (if *win32* "USERNAME" "USER")))

(message "Unimacs is powering up... Be patient, Master %s!" current-user)

(when (version< emacs-version "24.3")
  (error "Unimacs requires at least GNU Emacs 24.1, but you're running %s" emacs-version))

;; Always load newest byte code
(setq load-prefer-newer t)

(defvar unimacs-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Unimacs distribution.")
(defvar unimacs-core-dir       (expand-file-name "core" unimacs-dir)
  "The home of Unimacs's core functionality.")
(defvar unimacs-utils-dir      (expand-file-name  "utils" unimacs-dir)
  "This directory houses all of the utils files.")
(defvar unimacs-elpa-dir       (expand-file-name "packages/elpa" unimacs-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar unimacs-vendor-dir     (expand-file-name "packages/vendor" unimacs-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar unimacs-tempfiles-dir  (expand-file-name "tempfiles" unimacs-dir)
  "This folder stores all the automatically generated save/history-files.")
(defvar unimacs-personal-dir   (expand-file-name "personal" unimacs-dir)
  "This directory is for your personal configuration.
    Users of Emacs Unimacs are encouraged to keep their personal configuration
    changes in this directory.  All Emacs Lisp files there are loaded automatically
    by Unimacs.")
(defvar unimacs-configures-dir (expand-file-name  "personal/configures" unimacs-dir)
  "This directory houses all of the modules configure files.")
(defvar unimacs-personal-preload-dir (expand-file-name "preload" unimacs-personal-dir)
  "This directory is for your personal configuration, that you want loaded before Unimacs.")
(defvar unimacs-personal-postload-dir (expand-file-name "postload" unimacs-personal-dir)
  "This directory is for your personal configuration, that you want loaded after Unimacs. If you have some configurations different from official ones, please place in this directory.")

;; We don't add the tempfiles folders to git, so we need to create it at the first time.
(unless (file-exists-p unimacs-tempfiles-dir)
  (make-directory unimacs-tempfiles-dir t))
(unless (file-exists-p unimacs-elpa-dir)
  (make-directory unimacs-elpa-dir t))
(unless (file-exists-p unimacs-vendor-dir)
  (make-directory unimacs-vendor-dir t))
(unless (file-exists-p unimacs-personal-postload-dir)
  (make-directory unimacs-personal-postload-dir t))

(defun unimacs-add-subfolders-to-load-path (parent-dir)
 "Add all level PARENT-DIR subdirs to the `load-path'."
 (dolist (f (directory-files parent-dir))
   (let ((name (expand-file-name f parent-dir)))
     (when (and (file-directory-p name)
                (not (equal f ".."))
                (not (equal f ".")))
       (add-to-list 'load-path name)
       (unimacs-add-subfolders-to-load-path name)))))

;; add Unimacs's directories to Emacs's `load-path'
(add-to-list 'load-path unimacs-core-dir)
(add-to-list 'load-path unimacs-configures-dir)
(add-to-list 'load-path unimacs-elpa-dir)
(add-to-list 'load-path unimacs-vendor-dir)
(unimacs-add-subfolders-to-load-path unimacs-elpa-dir)
(unimacs-add-subfolders-to-load-path unimacs-vendor-dir)
(when my-benchmark
  (require 'benchmark-init)
  (benchmark-init/activate))

;; config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" unimacs-personal-dir))

;; preload the personal settings from `unimacs-personal-preload-dir'
(when (file-exists-p unimacs-personal-preload-dir)
  (message "Loading personal preload configuration files in %s..." unimacs-personal-preload-dir)
  (mapc 'load (directory-files unimacs-personal-preload-dir 't "^[^#]*\.el$")))

(message "Loading Unimacs's core...")
;; the core stuff
(require 'unimacs)

;; load the personal settings (this includes `custom-file')
(message "Loading personal modules...")
(when (file-exists-p unimacs-personal-dir)
  (message "Loading personal configuration files in %s..." unimacs-personal-dir)
  (load (expand-file-name "select-packages.el" unimacs-personal-dir))
  (load (expand-file-name "custom.el" unimacs-personal-dir)))

;; preload the personal settings from `unimacs-personal-postload-dir'
(when (file-exists-p unimacs-personal-postload-dir)
  (message "Loading personal postload configuration files in %s..." unimacs-personal-postload-dir)
  (mapc 'load (directory-files unimacs-personal-postload-dir 't "^[^#]*\.el$")))

(message "Unimacs rocks, Master %s!" current-user)
(when my-benchmark
  (benchmark-init/deactivate)
  (benchmark-init/show-durations-tree))

;;; init.el ends here
