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

(defun unimacs-join-lines ()
  "Join whole buffer or region."
  (interactive)
  (let* ((positions (unimacs-get-positions-of-buffer-or-region))
         (beg (car positions))
         (end (cdr positions)))
    (save-excursion
      (goto-char beg)
      (while (and (< (point) end)
                  (search-forward-regexp "[ \t]*\n[ \t]*" nil t))
        (replace-match "")))))

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
  (when (/= arg 1) (forward-line (1- arg)))

  (let ((orig-point (point)))
    (if visual-line-mode
        ;; Move by visual
        (progn
          (beginning-of-visual-line)
          (search-forward-regexp "^[\s\t]*" (line-end-position) t)
          (when (= orig-point (point))
            (beginning-of-visual-line)))
      ;; Not move by visual
      (back-to-indentation)
      (when (= orig-point (point))
        (beginning-of-line)))))

(defun unimacs-move-end-of-line (arg)
  "Move to the end of line, if `visual-line-mode' is on, then goto

the end of the visual line. If word wrap is turned off, it goes to
the beginning of next visual line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1) (forward-line (1- arg)))

  (if visual-line-mode
      ;; Move by visual, there's a bug in visual-line-mode.
      ;; If word-wrap is turned off, it goes to the beginning of next line when wrapped.
      (progn
        (end-of-visual-line)
        (unless (or (= (point) (line-end-position)) word-wrap)
          (backward-char)))
    ;; Not move by visual
    (end-of-line)))

(defun unimacs-scroll-up-line (&optional arg)
  "Scroll up 1 line."
  (interactive "^p")
  (or arg (setq arg 1))
  (scroll-up-line arg)
  (next-line arg))

(defun unimacs-scroll-down-line (&optional arg)
  "Scroll down 1 line."
  (interactive "^p")
  (or arg (setq arg 1))
  (scroll-down-line arg)
  (previous-line arg))

(defun unimacs-indent-current-line-or-region ()
  "Indent the currently visited buffer."
  (interactive)
  (let* ((positions (unimacs-get-positions-of-line-or-region))
         (beg (car positions))
         (end (cdr positions)))
    (indent-region beg end))
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

(defun unimacs-copy-file-path-to-clipboard ()
  "Copy the current buffer file path to the clipboard."
  (interactive)
  (let ((filepath (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filepath
      (kill-new filepath)
      (message "Copied buffer file path '%s' to the clipboard." filepath))))

(defun unimacs-get-positions-of-buffer-or-region ()
  "Return positions (beg . end) of the current buffer or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (if mark-active
        (setq beg (line-beginning-position))
      (setq beg (point-min)))
    (if mark-active
        (progn (exchange-point-and-mark)
               (setq end (line-end-position)))
      (setq end (point-max)))
    (cons beg end)))

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
  (let ((filepath (buffer-file-name)))
    (if (not (and filepath (file-exists-p filepath)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filepath)))
        (cond
         ((vc-backend filepath) (vc-rename-file filepath new-name))
         (t
          (rename-file filepath new-name t)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)))))))

(defun unimacs-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filepath (buffer-file-name)))
    (when filepath
      (if (vc-backend filepath)
          (vc-delete-file filepath)
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filepath))
          (delete-file filepath delete-by-moving-to-trash)
          (message "Deleted file %s" filepath)
          (kill-buffer))))))

(defun unimacs-copy-file-and-rename-buffer ()
  "Copy the current buffer and file it is visiting.

If the old file is under version control, the new file is added into
version control automatically"
  (interactive)
  (let ((filepath (buffer-file-name)))
    (if (not (and filepath (file-exists-p filepath)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filepath)))
        (copy-file filepath new-name t)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)
        (when (vc-backend filepath)
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
  "Byte-compile all your configuration files again.

With a prefix ARG, force recompile all files."
  (interactive
   (list (when current-prefix-arg (setq force t))))
  (when force (unimacs-clear-user-elcs))
  (dolist (dir (list unimacs-core-dir unimacs-configures-dir))
    (byte-recompile-directory dir 0 force))
  (message "Recompile the configuration files!"))

(defun unimacs-clear-user-elcs ()
  "Delete all the byte-compiled configuration files."
  (interactive)
  (dolist (dir (list unimacs-core-dir unimacs-configures-dir))
    (mapc 'delete-file (directory-files dir 't "\.elc$")))
  (remove-hook 'kill-emacs-hook 'unimacs-recompile-user-files)
  (message "The .elc files have been deleted!"))

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

(defun unimacs-create-elisp-scratch ()
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

(defun unimacs-open-configure-dir ()
  "Open the configuration dir to edit configuration files!"
  (interactive)
  (find-file unimacs-configures-dir))

(defun unimacs-toggle-list-bookmarks ()
  (interactive)
  (if (equal "*Bookmark List*" (buffer-name))
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
  (interactive (list (read-char "Goto char forward: " t)
                     current-prefix-arg))
  (unless (= (point) (line-end-position))
    (goto-char (1+ (point)))
    (search-forward (char-to-string char) (line-end-position) t arg)
    (goto-char (1- (point)))))

(defun unimacs-goto-char-current-line-backward (char &optional arg)
  "Jump backward to the first CHAR in current line.
The numth of occurences is determined by ARG."
  (interactive (list (read-char "Goto char backward: " t)
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

(defun unimacs-copy-func ()
  "Copy the function content."
  (interactive)
  (save-excursion
    (mark-defun)
    (whole-line-or-region-kill-ring-save nil))
  (message "The function has been copied!"))

(defun unimacs-copy-whole-buffer ()
  "Copy the whole beffer content."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (set-mark (point-max))
    (whole-line-or-region-kill-ring-save nil))
  (message "The whole buffer has been copied!"))

(defun unimacs-open-current-buffer-directory ()
  "Open current buffer directory under Windows system."
  (interactive)
  (shell-command (concat "explorer.exe /e, \"" (replace-regexp-in-string "/" "\\\\" (f-dirname (buffer-file-name))) "\"")))

(defun unimacs-describe-command-key (command key-map)
  "Describe the COMMAND key."
  (let* ((keys (where-is-internal (intern command) key-map))
         (keys (mapconcat (if (fboundp 'naked-key-description)
                              #'naked-key-description
                            #'key-description)
                          keys "', `")))
    (print keys)
    ))

(defun unimacs-update-msys64-bins (&optional dir)
  "Update utils/extra-bins/msys64/bin exe and dll from local Msys64."
  (interactive)
  (setq dir (or dir "c:/msys64"))
  (unless (file-exists-p dir)
    (set dir (file-name-as-directory (read-directory-name "Msys64 root dir: " nil nil t))))
  (when (file-exists-p dir)
    (let ((cur-bin-dir (expand-file-name "msys64/bin" unimacs-extra-bin-dir))
          (msys64-bin-dir (expand-file-name "usr/bin" dir))
          (mingw64-bin-dir (expand-file-name "mingw64/bin" dir))
          file1 file1-name file1-size file2 file2-size)
      (dolist (file1 (directory-files cur-bin-dir t "[a-zA-Z]"))
        (setq file1-name (file-name-nondirectory file1))
        (setq file1-size (file-attribute-size (file-attributes file1)))
        (setq file2 (expand-file-name file1-name msys64-bin-dir))
        (unless (file-exists-p file2)
          (setq file2 (expand-file-name file1-name mingw64-bin-dir)))
        (when (and (file-exists-p file2)
                   (setq file2-size (file-attribute-size (file-attributes file2)))
                   (not (= file1-size file2-size)))
          (copy-file file2 file1 t))))))

(provide 'unimacs-funcs)
;;; unimacs-funcs.el ends here
