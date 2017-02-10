;;; unimacs-keybindings.el --- Emacs Unimacs: some useful keybindings.
;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Lots of useful keybindings.

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
(require 'bind-key)

(unbind-key "C-\\")                ; toggle input method, annoy.
(bind-key "C-x p"   'proced)       ; Start proced in a similar manner to dired
(bind-key "C-x m"   'eshell)       ; Start eshell or switch to it if it's active.
(bind-key "C-x M"   '(lambda () (interactive) (eshell t))); Force start new eshell
(bind-key "C-x C-b" 'ibuffer)      ; replace buffer-menu with ibuffer
(bind-key "C-x g"   'magit-status)
(bind-key "C-x M-g" 'magit-dispatch-popup)

(bind-key* "<C-return>" 'other-window)

(bind-key "M-g c" 'goto-char)
(bind-key "M-g l" 'goto-line)
(bind-key "M-g f" 'unimacs-goto-char-current-line)
(bind-key "M-g b" 'unimacs-goto-char-current-line-backward)

(bind-key "RET" 'newline-and-indent)

(bind-key "A"   'apropos              'help-command); apropos-command (C-h a)
(bind-key "C-f" 'find-function        'help-command)
(bind-key "C-k" 'find-function-on-key 'help-command)
(bind-key "C-v" 'find-variable        'help-command)
(bind-key "C-l" 'find-library         'help-command)
(bind-key "C-i" 'info-display-manual  'help-command)

(bind-key "<f2>"  'repeat-complex-command)
(bind-key "<f10>" 'toggle-frame-maximized)
(bind-key "<f11>" 'toggle-frame-fullscreen)
(bind-key "<f12>" 'menu-bar-mode); toggle menu-bar visibility

(bind-key "M-<f12>"  'unimacs-toggle-list-bookmarks)

(bind-key "M-%"   'anzu-query-replace)
(bind-key "C-M-%" 'anzu-query-replace-regexp)

;; unimacs funs bindings.
(bind-key "C-<backspace>"   'unimacs-kill-line-backward)
(bind-key "C-S-<backspace>" 'unimacs-kill-whole-line)
(bind-key "C-a"             'unimacs-move-beginning-of-line)
(bind-key "C-M-\\"          'unimacs-indent-current-line-or-region)
(bind-key "C-M-|"           'unimacs-cleanup-buffer)
(bind-key "C-S-o"           'unimacs-smart-open-line-above)
(bind-key "C-o"             'unimacs-smart-open-line-below)
(bind-key* "M-j"            'unimacs-join-next-line)
(bind-key "M-S-j"           'unimacs-join-to-previous-line)
(bind-key "C-j"             'newline-and-indent)
(bind-key "C-S-j"           'backward-delete-char-untabify)
(bind-key* "M-0"            'unimacs-switch-to-previous-buffer)
(bind-key "C-x C-x"         'unimacs-exchange-point-and-mark)
(bind-key "C-x M-S-k"       'unimacs-delete-file-and-buffer)
(bind-key "C-x M-S-r"       'unimacs-rename-file-and-buffer)
(bind-key "C-x M-S-c"       'unimacs-copy-file-and-rename-buffer)
(bind-key "C-x M-p"         'unimacs-copy-file-name-to-clipboard)
(bind-key "C-S-M-h"         'unimacs-copy-func)
(bind-key "C-x H"           'unimacs-copy-whole-buffer)

(provide 'unimacs-keybindings)
;;; unimacs-keybindings.el ends here
