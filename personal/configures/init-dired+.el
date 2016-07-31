;;; init-dired+.el --- enhanced dired
;;; Commentary:
;; comments

;;; Code:
(use-package dired-details
  :init
  )
(require 'dired-details)
(dired-details-install)
;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

(defun diredext-exec-git-command-in-shell (command &optional arg file-list)
  "Run a shell command `git COMMAND`' on the marked files.
if no files marked, always operate on current line in dired-mode
"
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command "git command on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  (unless (string-match "[*?][ \t]*\\'" command)
    (setq command (concat command " *")))
  (setq command (concat "git " command))
  (dired-do-shell-command command arg file-list)
  (message command))

(eval-after-load 'dired
  '(progn
     ;; {dired-details
     (setq-default dired-details-hidden-string "")
     (define-key dired-mode-map "(" 'dired-details-toggle)
     (define-key dired-mode-map ")" 'dired-details-toggle)
     ;; }
     (define-key dired-mode-map "/" 'diredext-exec-git-command-in-shell)

     (require 'dired+)
     (setq dired-recursive-deletes 'top)
     (define-key dired-mode-map [mouse-2] 'dired-find-file)
     (dolist (file `(((if *unix* "zathura" "open") "pdf" "dvi" "pdf.gz" "ps" "eps")
                     ("unrar x" "rar")
                     ((if *unix* "mplayer -stop-xscreensaver" "mplayer")  "avi" "mpg" "rmvb" "rm" "flv" "wmv" "mkv" "mp4" "m4v" "webm")
                     ("mplayer -playlist" "list" "pls")
                     ((if *unix* "feh" "open") "gif" "jpeg" "jpg" "tif" "png" )
                     ("7z x" "7z")
                     ("djview" "djvu")
                     ("firefox" "xml" "xhtml" "html" "htm" "mht")))
       (add-to-list 'dired-guess-shell-alist-default
                    (list (concat "\\." (regexp-opt (cdr file) t) "$")
                          (car file))))
     ))

(require 'dired-x)
(setq dired-omit-files
      (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
              (seq "~" eol)                 ;; backup-files
              (seq bol ".svn" eol)           ;; svn dirs
              (seq ".pyc" eol)
              (seq bol "GTAGS" eol)
              (seq bol "GRTAGS" eol)
              (seq bol "GPATH" eol)
              ))
      )
(setq dired-omit-extensions
      (append dired-latex-unclean-extensions
              dired-bibtex-unclean-extensions
              dired-texinfo-unclean-extensions))
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
(put 'dired-find-alternate-file 'disabled nil)

(provide 'init-dired+)
;;; init-dired+.el ends here