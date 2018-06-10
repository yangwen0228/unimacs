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
(setq use-package-always-ensure t)
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(use-package use-package-chords
  :config (key-chord-mode 1))

(use-package anzu
  :diminish anzu-mode
  :init (setq query-replace-skip-read-only t)
  :config (global-anzu-mode t))

(use-package autorevert
  ;; revert buffers automatically when underlying files are changed externally
  :diminish auto-revert-mode
  :functions auto-revert-mode
  :hook (find-file . auto-revert-mode)
  :init
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil
        revert-without-query '(".*")))

(use-package browse-kill-ring
  :bind ("M-y" . browse-kill-ring)
  :config
  ;; restore windows: save window configuration before execution
  (defadvice browse-kill-ring (before browse-kill-ring-fullscreen activate)
         (window-configuration-to-register :browse-kill-ring-fullscreen))
  ;; override: support multiple-cursors
  (defun browse-kill-ring-insert-and-quit()
    "Insert the selected text at all cursors"
    (interactive)
    (let* ((buf (current-buffer))
           (text (browse-kill-ring-current-string buf (point)))
           (command (lambda () (interactive) (when text (save-excursion (insert text))))))
      (switch-to-buffer browse-kill-ring-original-buffer)
      (when (and (boundp 'multiple-cursors-mode) multiple-cursors-mode)
        ;; Execute the command for each fake cursor
        (mc/execute-command-for-all-fake-cursors command))
      (switch-to-buffer buf))
    ;; Finally execute the command for current cursor
    (browse-kill-ring-insert t)
    ;; restore windows
    (delete-other-windows)
    (jump-to-register :browse-kill-ring-fullscreen)))

(use-package diff-hl
  :init
  (setq diff-hl-side 'left)
  :hook
  ((prog-mode magit-mode) . diff-hl-mode)
  (dired-mode . diff-hl-dired-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package dizzee
  :config
  (dz-defservice gifcap "licecap.exe")
  (dz-defservice keycastow "keycastow.exe")
  (dz-defservice-group unimacs-gifcap-keycastow
                       (gifcap keycastow))
  (defadvice gifcap-start (before gifcap-start activate)
    ;; set emacs position and size
    ;; licecap size and position is in utils/extra-bins/gifcap/bin/licecap.ini
    ;; keycastow position is in utils/extra-bins/gifcap/keycastow.ini
    (set-frame-position nil 300 100)
    (set-frame-size     nil 622 500 t))
  (defun dz-comint-pop (name command &optional args dont-pop)
    "Make a comint buffer for process `name', executing `command' with
`args' and then pop to that buffer."
    (ansi-color-for-comint-mode-on)
    (apply 'make-comint name command nil args))

  (defun dz-subp-stop (name)
    "Check to see if the process `name' is running stop it if so."
    (let ((proc (get-buffer-process (concat "*" name "*"))))
      (when proc (delete-process proc))
      (when (get-buffer (concat "*" name "*"))
        (kill-buffer (concat "*" name "*"))))))

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
  (setq-default ediff-split-window-function 'split-window-horizontally
                ediff-window-setup-function 'ediff-setup-windows-plain)
  :hook (ediff-quit . my-kill-ediff-buffers)
  :config (add-hook 'ediff-after-quit-hook-internal 'winner-undo))

(use-package eldoc :ensure nil
  :init (setq eldoc-idle-delay 0.2 eldoc-echo-area-use-multiline-p t)
  :config (eldoc-mode t)
  :diminish (eldoc-mode))

(use-package expand-region
  :commands (er/expand-region er/mark-symbol)
  :bind ("C-=" . er/expand-region))

(use-package hl-line :ensure nil
  ;; highlight current line
  :config (global-hl-line-mode 1))

(use-package mic-paren
  ;; highlight contents between braces.
  :init
  (setq paren-sexp-mode t paren-match-face 'highlight)
  :config
  (paren-activate)
  ;; Conflict with multiple-cursors and active mark:
  (defadvice mic-paren-highlight (after mic-paren-highlight activate)
    (if (or mark-active (and (boundp 'multiple-cursors-mode)
                             multiple-cursors-mode))
        (progn (mic-delete-overlay (aref mic-paren-overlays 0))
               (mic-delete-overlay (aref mic-paren-overlays 1))
               (mic-delete-overlay (aref mic-paren-overlays 2))))))

(use-package midnight :ensure nil
  ;; midnight mode purges buffers every midnight-period time.
  :config
  (midnight-mode t)
  (midnight-delay-set 'midnight-delay "9:00") ; start time or seconds to 24:00
  (setq clean-buffer-list-delay-general 7)    ; clean buffers exceeding 7 day.
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
        (move-to-column column t))))))

(use-package nlinum
  :if (< emacs-major-version 26)
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
  :diminish page-break-lines-mode
  :config (global-page-break-lines-mode))

(use-package popup
  :commands popup-mode popup-tip
  :custom-face (popup-tip-face ((t (:background "orange3" :foreground "black")))))

(use-package rainbow-mode :commands rainbow-mode)

(use-package scratch)

(use-package server :ensure nil
  ;; edit server, must use :config. cannot use :init.
  :config
  (unless (file-exists-p server-auth-dir)
    (make-directory server-auth-dir))
  (unless (or (not server-socket-dir) (file-exists-p server-socket-dir))
    (make-directory server-socket-dir))
  (defun server-ensure-safe-dir (dir) "Noop" t)
  (server-start))

(use-package smart-tab
  :diminish smart-tab-mode
  :config
  ;; NOTICE: Don't manually bind to 'smart-tab, otherwise, dead loop.
  (unbind-key "<tab>")
  (global-smart-tab-mode 1)
  (setq smart-tab-disabled-major-modes
        (remove 'org-mode smart-tab-disabled-major-modes)) ; org-mode: yasnippet
  (setq smart-tab-completion-functions-alist nil)
  ;; Never use auto-complete and hippie-expand, use yas-expand, so override:
  (defun smart-tab-call-completion-function ()
    "Get a completion function according to current major mode."
    (let ((completion-function
           (cdr (assq major-mode smart-tab-completion-functions-alist))))
      (if completion-function
          (funcall completion-function)
        (if (not (minibufferp))
            (unless (yas-expand)
              (call-interactively 'company-yasnippet)))))))

(use-package so-long :ensure nil
  ;; When the lines in a buffer are so long that performance could suffer.
  :init (setq so-long-threshold 500)
  :config (so-long-enable))

(use-package subword :ensure nil
  :diminish subword-mode
  ;; M-f better jump between camel words. C-M-f whole word.
  :init (add-hook 'prog-mode-hook #'subword-mode))

(use-package tramp :ensure nill :disabled
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
  ;; visual-line-mode: manipulate lines by visual mode.
  :diminish visual-line-mode
  :init
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
  (add-hook 'visual-line-mode-hook '(lambda () (toggle-word-wrap) (message "")))
  :config (global-visual-line-mode 1))

(use-package highlight-symbol
  :bind (("M-*" . highlight-symbol))
  :hook (prog-mode . highlight-symbol-nav-mode))

(use-package view :ensure nil
  :init
  ;; view-mode
  (custom-set-variables '(view-read-only t))
  (with-eval-after-load 'view
    (define-key view-mode-map (kbd "N") 'View-search-last-regexp-backward)
    (define-key view-mode-map (kbd "?") 'View-search-regexp-backward)
    (define-key view-mode-map (kbd "g") 'View-goto-line)
    (define-key view-mode-map (kbd "w") 'forward-word)
    (define-key view-mode-map (kbd "W") 'forward-symbol)
    (define-key view-mode-map (kbd "b") 'backward-word)
    (define-key view-mode-map (kbd "h") 'backward-char)
    (define-key view-mode-map (kbd "n") 'next-line)
    (define-key view-mode-map (kbd "p") 'previous-line)
    (define-key view-mode-map (kbd "l") 'forward-char)
    (define-key view-mode-map (kbd "[") 'backward-paragraph)
    (define-key view-mode-map (kbd "]") 'forward-paragraph))
  (with-eval-after-load 'doc-view
    (define-key doc-view-mode-map (kbd "n") 'doc-view-next-line-or-next-page)
    (define-key doc-view-mode-map (kbd "p") 'doc-view-previous-line-or-previous-page)))

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  ;; highlight when undo or yank
  :config (volatile-highlights-mode 1))

(use-package whitespace
  :diminish (global-whitespace-mode whitespace-mode whitespace-newline-mode)
  ;; display special chars, like tabs and trailing whitespace.
  ;; tab-mark lead to freeze Emacs!
  :init
  (setq-default show-trailing-whitespace t
                indicate-empty-lines t
                indicate-buffer-boundaries 'left)
  (setq whitespace-line-column 120
        whitespace-style '(face trailing tabs)) ; lines-tail
  (setq whitespace-display-mappings
        '(;; (newline-mark 10 [172 10]) ; 10 LINE FEED
          ;; (tab-mark 9 [8680 9] [92 9]); 9 TAB
          ))
  :config (global-whitespace-mode 1))

(use-package winner :defer 0
  ;; window layout undo/redo
  :bind (("C-," . winner-undo)
         ("C-." . winner-redo))
  ;; always load
  :init (setq winner-ring-size 10)
  :config (winner-mode 1))

(use-package whole-line-or-region
  :diminish whole-line-or-region-mode
  ;; kill or yank a whole line.
  :bind (("C-w" . whole-line-or-region-kill-region)
         ("M-w" . whole-line-or-region-kill-ring-save)
         ("C-y" . whole-line-or-region-yank)))

(use-package benchmark-init
  :commands (benchmark-init/activate))

(provide 'unimacs-modes)
;;; unimacs-modes.el ends here
