;;; unimacs-packages.el --- Emacs Unimacs: default package selection.
;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Takes care of the automatic installation of all the packages required by
;; Emacs Unimacs.

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
(require 'cl)
(require 'package)

(defun unimacs-choose-elpa-source (name)
  "Choose the right elpa source by NAME : melpa/popkit.

Because in China mainland, sometimes melpa doesn't work!"
  (interactive
   (list (intern (completing-read "Elpa name (default melpa): "
                                  '("melpa" "popkit")
                                  nil t nil nil "melpa"))))
  (unimacs-create-package-archives name)
  (package-list-packages))

(defun unimacs-create-package-archives (name)
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")))
  (cl-ecase name
    (melpa
     (add-to-list 'package-archives
                  '("melpa" . "http://melpa.milkbox.net/packages/") t))
    (popkit
     (add-to-list 'package-archives
                  '("popkit" . "http://elpa.popkit.org/packages/") t)
     (add-to-list 'package-archives
                  '("ELPA" . "http://tromey.com/elpa/") t)
     (add-to-list 'package-archives
                  '("SC"   . "http://joseito.republika.pl/sunrise-commander/") t)
     )))

(unimacs-create-package-archives 'popkit)

;; ;; set package-user-dir to be relative to Unimacs install path
(setq package-user-dir unimacs-elpa-dir)

(package-initialize)

;; some packages must be loaded, and should not be maintained by selectpackages.el
(defvar unimacs-packages
  '(
    ;; packages:
    use-package
    bind-key
    diminish
    solarized-theme

    browse-kill-ring
    diff-hl                ;display svn git status at the left margin
    easy-kill              ; mark and copy whole lines, like V in Vim.
    elisp-slime-nav
    epl ; Emacs package library based on package.el
    gitconfig-mode gitignore-mode
    google-c-style
    magit
    move-text ; move a line up/down, alt-up alt-down
    rainbow-mode
    popup
    ;; libs:
    dash f s
    )
  "A list of packages to ensure are installed at launch.")

(defun unimacs-packages-installed-p ()
  "Check if all packages in `unimacs-packages' are installed."
  (every #'package-installed-p unimacs-packages))

(defun unimacs-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package unimacs-packages)
    (add-to-list 'unimacs-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun unimacs-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'unimacs-require-package packages))

(defun unimacs-install-packages ()
  "Install all packages listed in `unimacs-packages'."
  (unless (unimacs-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs Unimacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (unimacs-require-packages unimacs-packages)))

;; run package installation
(unimacs-install-packages)

(defun unimacs-list-foreign-packages ()
  "Browse third-party packages not bundled with Unimacs.

Behaves similarly to `package-list-packages', but shows only the packages that
are installed and are not in `unimacs-packages'.  Useful for
removing unwanted packages."
  (interactive)
  (package-show-package-list
   (set-difference package-activated-list unimacs-packages)))

(defmacro unimacs-auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present.  The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

(defvar unimacs-auto-install-alist
  '(("\\.clj\\'"           clojure-mode    clojure-mode)
    ("\\.cmake\\'"         cmake-mode      cmake-mode)
    ("CMakeLists\\.txt\\'" cmake-mode      cmake-mode)
    ("\\.coffee\\'"        coffee-mode     coffee-mode)
    ("\\.css\\'"           css-mode        css-mode)
    ("\\.csv\\'"           csv-mode        csv-mode)
    ("\\.d\\'"             d-mode          d-mode)
    ("\\.dart\\'"          dart-mode       dart-mode)
    ("\\.elm\\'"           elm-mode        elm-mode)
    ("\\.ex\\'"            elixir-mode     elixir-mode)
    ("\\.exs\\'"           elixir-mode     elixir-mode)
    ("\\.elixir\\'"        elixir-mode     elixir-mode)
    ("\\.erl\\'"           erlang          erlang-mode)
    ("\\.feature\\'"       feature-mode    feature-mode)
    ("\\.go\\'"            go-mode         go-mode)
    ("\\.groovy\\'"        groovy-mode     groovy-mode)
    ("\\.haml\\'"          haml-mode       haml-mode)
    ("\\.hs\\'"            haskell-mode    haskell-mode)
    ("\\.json\\'"          json-mode       json-mode)
    ("\\.kv\\'"            kivy-mode       kivy-mode)
    ("\\.latex\\'"         auctex          LaTeX-mode)
    ("\\.less\\'"          less-css-mode   less-css-mode)
    ("\\.lua\\'"           lua-mode        lua-mode)
    ("\\.markdown\\'"      markdown-mode   markdown-mode)
    ("\\.md\\'"            markdown-mode   markdown-mode)
    ("\\.ml\\'"            tuareg          tuareg-mode)
    ("\\.pp\\'"            puppet-mode     puppet-mode)
    ("\\.php\\'"           php-mode        php-mode)
    ("\\.proto\\'"         protobuf-mode   protobuf-mode)
    ("\\.pyd\\'"           cython-mode     cython-mode)
    ("\\.pyi\\'"           cython-mode     cython-mode)
    ("\\.pyx\\'"           cython-mode     cython-mode)
    ("PKGBUILD\\'"         pkgbuild-mode   pkgbuild-mode)
    ("\\.rs\\'"            rust-mode       rust-mode)
    ("\\.sass\\'"          sass-mode       sass-mode)
    ("\\.scala\\'"         scala-mode2     scala-mode)
    ("\\.scss\\'"          scss-mode       scss-mode)
    ("\\.slim\\'"          slim-mode       slim-mode)
    ("\\.styl\\'"          stylus-mode     stylus-mode)
    ("\\.swift\\'"         swift-mode      swift-mode)
    ("\\.textile\\'"       textile-mode    textile-mode)
    ("\\.thrift\\'"        thrift          thrift-mode)
    ("\\.yml\\'"           yaml-mode       yaml-mode)
    ("\\.yaml\\'"          yaml-mode       yaml-mode)
    ("Dockerfile\\'"       dockerfile-mode dockerfile-mode)))

;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually if it's already installed
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'"       . markdown-mode)))

(when (package-installed-p 'pkgbuild-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD\\'"    . pkgbuild-mode)))

;; build auto-install mappings
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (unimacs-auto-install extension package mode))))
 unimacs-auto-install-alist)

(defun is-older-version (file-name all-folders)
  (cl-block checked
    (let ((pattern "^\\(.+?\\)-\\([0-9]+.[0-9]+\\)$"))
      (if (string-match pattern file-name)
          (let ((name (match-string 1 file-name))
                (time (match-string 2 file-name)))
            (dolist (file-name1 all-folders)
              (if (string-match pattern file-name1)
                  (let ((name1 (match-string 1 file-name1))
                        (time1 (match-string 2 file-name1)))
                    (if (and (string= name name1) (string< time time1))
                        (return-from checked t)))))))
      nil)))
(defun delete-old-duplicate-versions ()
  (let ((all-folders (directory-files unimacs-elpa-dir)))
    (dolist (file-name all-folders)
      (if (is-older-version file-name all-folders)
          (delete-directory (expand-file-name file-name unimacs-elpa-dir) t nil))
      )))
(delete-old-duplicate-versions)


(provide 'unimacs-packages)
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; unimacs-packages.el ends here
