;;; unimacs-funcs.el --- Emacs Unimacs: Core Unimacs functions.
;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Here are the definitions of most of the functions added by Unimacs.

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

(require 'thingatpt)
(require 'dash)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(defun unimacs-open-with (arg)
  "Open visited file in default external program.

With a prefix ARG always prompt for command to use."
  (interactive "P")
  (when buffer-file-name
    (start-process "unimacs-open-with-process"
                   "*unimacs-open-with-output*"
                   (cond
                    ((and (not arg) (eq system-type 'darwin)) "open")
                    ((and (not arg) (member system-type '(gnu gnu/linux gnu/kfreebsd))) "xdg-open")
                    (t (read-shell-command "Open current file with: ")))
                   (shell-quote-argument buffer-file-name))))

(defun unimacs-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun unimacs-smart-open-line-below ()
  "Insert an empty line below the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun unimacs-join-next-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

(defun unimacs-join-to-previous-line ()
  "Join the current line to the line above it."
  (interactive)
  (delete-indentation))

(defun unimacs-kill-whole-line (&optional arg)
  "A simple wrapper around command `kill-whole-line' that respects indentation.
Passes ARG to command `kill-whole-line' when provided."
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation))

(defun unimacs-kill-line-backward ()
  "Kill line before current point, the opposite and complement of 'kill-line."
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))

(defun unimacs-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun unimacs-indent-current-line-or-region ()
  "Indent the currently visited buffer."
  (interactive)
  (if mark-active
      (indent-region (point-min) (point-max))
    (indent-region (line-beginning-position) (line-end-position)))
  (message "Indent lines...done"))

(defun unimacs-indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max))
  (message "Indent whole buffer...done"))

(defun unimacs-annotate-todo ()
  "Put fringe marker on TODO: lines in the curent buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (format "[[:space:]]*%s+[[:space:]]*TODO:" comment-start) nil t)
      (let ((overlay (make-overlay (- (point) 5) (point))))
        (overlay-put overlay
                     'before-string
                     (propertize (format "A")
                                 'display '(left-fringe right-triangle)))))))

(defun unimacs-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun unimacs-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

(defun unimacs-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (unimacs-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (-dotimes arg
      (lambda (n)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point))))
    (goto-char (+ origin (* (length region) arg) arg))))

(defun unimacs-duplicate-and-comment-current-line-or-region (arg)
  "Duplicates and comments the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (unimacs-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (comment-or-uncomment-region beg end)
    (setq end (line-end-position))
    (-dotimes arg
      (lambda (n)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point))))
    (goto-char (+ origin (* (length region) arg) arg))))

(defun unimacs-rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)))))))

(defun unimacs-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(defun unimacs-copy-file-and-rename-buffer ()
  "Copy the current buffer and file it is visiting.

If the old file is under version control, the new file is added into
version control automatically"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (copy-file filename new-name t)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)
        (when (vc-backend filename)
          (vc-register))))))

(defun unimacs-untabify-buffer ()
  "Remove all tabs from the current buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun unimacs-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (unimacs-indent-buffer)
  (unimacs-untabify-buffer)
  (whitespace-cleanup))

(defun unimacs-recompile-user-files (&optional force)
  "Byte-compile all your dotfiles again.

With a prefix ARG, force recompile all files."
  (interactive
   (list (when current-prefix-arg (setq force t))))
  (byte-recompile-directory unimacs-core-dir 0 force)
  (byte-recompile-directory unimacs-configures-dir 0 force))

(defun unimacs-sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun unimacs-start-or-switch-to (function buffer-name)
  "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
Otherwise switch to the buffer named BUFFER-NAME.  Don't clobber
the current buffer."
  (if (not (get-buffer buffer-name))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (funcall function))
    (switch-to-buffer-other-window buffer-name)))

(defun unimacs-insert-date (prefix)
  "Insert the current date.

With PREFIX argument, use ISO format.
With two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%Y%m%d")
                 ((equal prefix '(4)) "%Y-%m-%d")
                 ((equal prefix '(16)) "%d %B %Y"))))
    (insert (format-time-string format))))

(defun unimacs-recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: "
                                   (-map 'abbreviate-file-name recentf-list)
                                   nil t)))
    (when file
      (find-file file))))

(defun unimacs-swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (if (/= (count-windows) 2)
      (message "You need exactly 2 windows to do this.")
    (let* ((w1 (car (window-list)))
           (w2 (cadr (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))
  (other-window 1))

(defun unimacs-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun unimacs-kill-other-normal-buffers ()
  "Kill all normal buffers but the current one.
Doesn't mess with special buffers."
  (interactive)
  (when (y-or-n-p "Are you sure you want to kill all normal buffers but the current one? ")
    (-each
        (->> (buffer-list)
             (-filter #'buffer-file-name)
             (--remove (eql (current-buffer) it)))
      #'kill-buffer)))

(defun unimacs-kill-other-special-buffers ()
  "Kill all special buffers but the current one.
Doesn't mess with normal buffers."
  (interactive)
  (when (y-or-n-p "Are you sure you want to kill all special buffers but the current one? ")
    (-each
        (->> (buffer-list)
             (-remove #'buffer-file-name)
             (--remove (eql (current-buffer) it)))
      #'kill-buffer)))

(defun unimacs-create-scratch-buffer ()
  "Create a new scratch buffer."
  (interactive)
  (progn
    (switch-to-buffer
     (get-buffer-create (generate-new-buffer-name "*scratch*")))
    (emacs-lisp-mode)))

(defun unimacs-eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.

    If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

(defun unimacs-exchange-point-and-mark ()
  "Identical to `exchange-point-and-mark' but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(require 'epl)

(defun unimacs-update ()
  "Update Unimacs to its latest version."
  (interactive)
  (when (y-or-n-p "Do you want to update Unimacs? ")
    (message "Updating installed packages...")
    (epl-upgrade)
    (message "Updating Unimacs...")
    (cd unimacs-dir)
    (shell-command "git pull")
    (unimacs-recompile-init)
    (message "Update finished. Restart Emacs to complete the process.")))

(defun unimacs-find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(defun unimacs-find-shell-init-file ()
  "Edit the shell init file in another window."
  (interactive)
  (let* ((shell (car (reverse (s-split "/" (getenv "SHELL")))))
         (shell-init-file (cond
                           ((s-equals? "zsh" shell) ".zshrc")
                           ((s-equals? "bash" shell) ".bashrc")
                           (t (error "Unknown shell")))))
    (find-file-other-window (expand-file-name shell-init-file (getenv "HOME")))))

(defun unimacs-toggle-list-bookmarks ()
  (interactive)
  (if (equalp "*Bookmark List*" (buffer-name))
      (quit-window)
    (call-interactively 'list-bookmarks)))

(defun unimacs-dos2unix ()
  "Convert a buffer from dos ^M end of lines to unix end of lines."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t) (replace-match ""))))

(defun unimacs-unix2dos ()
  "Convert a buffer from unix end of lines to dos ^M end of lines."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t) (replace-match "\r\n"))))

(defun unimacs-goto-char-current-line (char &optional arg)
  "Jump to the first CHAR in current line.
The numth of occurences is determined by ARG."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (unless (= (point) (line-end-position))
    (goto-char (1+ (point)))
    (search-forward (char-to-string char) (line-end-position) t arg)
    (goto-char (1- (point)))))

(defun unimacs-goto-char-current-line-backward (char &optional arg)
  "Jump backward to the first CHAR in current line.
The numth of occurences is determined by ARG."
  (interactive (list (read-char "char: " t)
                     current-prefix-arg))
  (search-backward (char-to-string char) (line-beginning-position) t arg))

(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (or (equal major-mode 'dired-mode)
              (and (buffer-file-name)
                   (not (file-exists-p (file-name-directory (buffer-file-name)))))
              (and (buffer-file-name)
                   (file-writable-p buffer-file-name)))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(provide 'unimacs-funcs)
;;; unimacs-funcs.el ends here
