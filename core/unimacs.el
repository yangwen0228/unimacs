;;; unimacs.el --- Emacs Unimacs: interface to other files.
;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;; load other file under this directory.

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
;; The order should not be changed!
(require 'unimacs-packages) ; must come first
(require 'unimacs-funcs)    ; must come second
(require 'unimacs-modes)    ; must come third
(require 'unimacs-ui)
(require 'unimacs-keybindings)
(require 'unimacs-tempfiles)
(require 'unimacs-settings)

(provide 'unimacs)
;;; unimacs.el ends here
