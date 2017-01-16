;;; tcl-hm-mode.el --- extension to Tcl mode for editing Synopsys HM scripts

;; Copyright (C) 2013, 2014 Yang Wen

;; Author:      Yang Wen <>
;; Maintainer:  Yang Wen <>
;; RCS:         $Id: tcl-hm-mode.el,v 1.0 2014/06/11 11:23:54 reto Exp reto $
;; RCS:         $Id: tcl-hm-mode.el,v 1.1 2015/03/11 11:23:54 reto Exp reto $
;; Keywords:    languages tcl hm

(defconst tcl-hm-version "1.1"
  "Tcl HM Mode version number.")

(defconst tcl-hm-time-stamp "2015-03-11"
  "Tcl HM Mode time stamp for last update.")

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This package provides an extension to Tcl major mode for editing scripts for
;; HyperMesh Tcl shell.
;; It adds the following features:

;;   - Syntax highlighting
;;   - Word/command completion
;;   - Block commenting

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Documentation

;; See comment string of function `tcl-hm-mode' or type `C-c C-h' in Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Installation:

;; Prerequisites:  GNU Emacs 20.X/21.X, XEmacs 20.X/21.X

;; Put `tcl-hm-mode.el' into the `site-lisp' directory of your Emacs
;; installation or into an arbitrary directory that is added to the load path
;; by the following line in your Emacs start-up file (`.emacs'):

;;   (setq load-path (cons (expand-file-name "<directory-name>") load-path))

;; If you already have the compiled `tcl-hm-mode.elc' file, put it in the same
;; directory.  Otherwise, byte-compile the source file:
;;   Emacs:  M-x byte-compile-file  ->  tcl-hm-mode.el
;;   Unix:   emacs -batch -q -no-site-file -f batch-byte-compile tcl-hm-mode.el

;; Add the following lines to the `site-start.el' file in the `site-lisp'
;; directory of your Emacs installation or to your Emacs start-up file
;; (`.emacs'):

;;   (autoload 'tcl-hm-mode "tcl-hm-mode" "Tcl HM Mode" t)
;;   (add-to-list 'auto-mode-alist '("\\.tcl\\'" . tcl-hm-mode))

;; In the last line the pattern for matching a file name or file extension can
;; be changed to any naming convention for HM script files.  Also, the last
;; line can be omitted and Tcl HM Mode be automatically invoked only for files
;; that have the following pattern on the first line:

;;   # -*- tcl-hm -*-

;; Alternatively, the following line can be used to always load Tcl HM Mode
;; along with Tcl mode:

;;   (add-hook 'tcl-mode-hook '(lambda () (require 'tcl-hm-mode) (tcl-hm-mode)))

;; Finally, Tcl HM Mode can also be turned on and off manually using the
;; command `M-x tcl-hm-mode'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup tcl-hm nil
  "Customizations for tcl-hm Minor Mode."
  :group 'languages)

(defcustom tcl-hm-intelligent-tab t
  "*Non-nil means `TAB' does indentation, word completion and tab insertion.
That is, if preceeding character is part of a word then complete word,
else if not at beginning of line then insert tab,
else if last command was a `TAB' or `RET' then dedent one step,
else indent current line.
If nil, TAB always indents current line."
  :type 'boolean
  :group 'tcl-hm)

(defcustom tcl-hm-extra-commands nil
  "List of additional HM commands for highlighting and completion."
  :type '(repeat (string :tag "Command"))
  :group 'tcl-hm)

(defcustom tcl-hm-extra-variables nil
  "List of additional HM variables for highlighting and completion."
  :type '(repeat (string :tag "Variable"))
  :group 'tcl-hm)

(defcustom tcl-hm-extra-attributes nil
  "List of additional HM attributes for highlighting and completion."
  :type '(repeat (string :tag "Attributes"))
  :group 'tcl-hm)

(defcustom tcl-hm-extra-highlight nil
  "Highlight extra keywords with special warning color."
  :type 'boolean
  :group 'tcl-hm)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; copy Tcl mode keymap
(require 'tcl)

;; Key bindings
(defvar tcl-hm-mode-map (copy-keymap tcl-mode-map)
  "Keymap for Tcl HM Mode.")

;; mode specific key bindings
(define-key tcl-hm-mode-map "\C-c\C-h" 'tcl-hm-doc-mode)
;;(define-key tcl-hm-mode-map "\C-c\C-c" 'tcl-hm-comment-uncomment-region)
(define-key tcl-hm-mode-map "\C-c\C-c" 'comment-dwim)
;; add electric key bindings
(define-key tcl-hm-mode-map (kbd "<C-tab>") 'tcl-hm-electric-tab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table

(defvar tcl-hm-he-syntax-table (copy-syntax-table tcl-mode-syntax-table)
  "Syntax table used for `hippie-exp'.")

;; add '_' to syntax table
(modify-syntax-entry ?_ "w" tcl-hm-he-syntax-table)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keywords
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; adjust variable bindings size
(when (< max-specpdl-size 2000)
  (setq max-specpdl-size 2000))

(defconst tcl-hm-keywords
  '(
    "after" "alias" "append" "apropos" "array" "binary" "break" "catch" "cd"
    "clock" "close" "concat" "continue" "date"
    "echo" "else" "elseif" "encoding" "eof" "error"
    "error_info" "eval" "exec" "exit" "expr" "fblocked" "fconfigure" "fcopy"
    "file" "fileevent" "flush" "for" "foreach" "format" "getenv"
    "gets" "glob" "global" "help" "history" "if" "incr" "info"
    "interp" "join" "lappend" "lindex" "linsert" "list"
    "llength" "lminus" "lrange" "lreplace" "ls" "lsearch" "lset" "lsort" "man"
    "namespace" "open" "package" "pid"
    "printenv" "printvar" "proc" "proc_args" "proc_body" "puts" "quit"
    "read" "redirect" "regexp" "regsub" "rename" "return" "scan" "seek" "set"
    "setenv" "sh" "socket" "source" "split" "string" "subst" "switch" "tell" "then" "time"
    "trace" "unset" "update" "uplevel" "upvar"
    "variable" "vwait" "which" "while"
    )
  "List of Tcl keywords.")

(defvar tcl-hm-keywords-file-name
  (expand-file-name
   "tcl_hm.keywords"
   (file-name-directory load-file-name)))

(defvar tcl-hm-init-flag nil)

(defconst tcl-hm-commands
  (with-temp-buffer
    (insert-file-contents-literally tcl-hm-keywords-file-name)
    (split-string (buffer-string)))
  "List of HM commands (2014.06 release).")

(defconst tcl-hm-extra-commands
  '()
  "List of HM commands (2014.06 release).")

(defconst tcl-hm-variables
  '()
  "List of HM variables (2014.06 release).")

(defconst tcl-hm-attributes
  '()
  "List of HM attributes (2014.06 release).")

(setq tcl-hm-extra-highlight t)

;; `regexp-opt' undefined (`xemacs-devel' not installed)
(unless (fboundp 'regexp-opt)
  (defun regexp-opt (strings &optional paren)
    (let ((open (if paren "\\(" "")) (close (if paren "\\)" "")))
      (concat open (mapconcat 'regexp-quote strings "\\|") close))))

(defconst tcl-hm-keywords-regexp
  (concat "\\<\\(" (regexp-opt tcl-hm-keywords) "\\)\\>")
  "Regexp for Tcl keywords.")

(defconst tcl-hm-variables-regexp
  (concat "\\<\\(" (regexp-opt (append tcl-hm-variables
                                       (unless tcl-hm-extra-highlight
                                         tcl-hm-extra-variables))) "\\)\\>")
  "Regexp for HM variables.")

(defconst tcl-hm-attributes-regexp
  (concat "\\<\\(" (regexp-opt (append tcl-hm-attributes
                                       (unless tcl-hm-extra-highlight
                                         tcl-hm-extra-attributes))) "\\)\\>")
  "Regexp for HM attributes.")

(defconst tcl-hm-extra-commands-regexp
  (concat "\\<\\(" (regexp-opt (if tcl-hm-extra-highlight
                                   (append tcl-hm-extra-commands
                                           tcl-hm-extra-attributes)
                                 '(""))) "\\)\\>")
  "Regexp for extra commands.")

(defconst tcl-hm-extra-variables-regexp
  (concat "\\<\\(" (regexp-opt (if tcl-hm-extra-highlight
                                   tcl-hm-extra-variables
                                 '(""))) "\\)\\>")
  "Regexp for extra variables.")

(defface font-lock-hm-face
  '((((class grayscale) (background light)) :foreground "Gray90" :weight bold)
    (((class grayscale) (background dark))  :foreground "DimGray" :weight bold)
    (((class color) (min-colors 88) (background light)) :foreground "DeepPink")
    (((class color) (min-colors 88) (background dark))  :foreground "DarkOrange")
    (((class color) (min-colors 16) (background light)) :foreground "ForestGreen")
    (((class color) (min-colors 16) (background dark))  :foreground "PaleGreen")
    (((class color) (min-colors 8)) :foreground "green")
    (t :weight bold :underline t))
  "Font Lock mode face used to highlight hm commands."
  :group 'font-lock-faces)

(defvar font-lock-hm-face 'font-lock-hm-face)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fontification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq tcl-font-lock-keywords
      (list
       ;; highlight function names
       '("\\<proc\\s-+\\([^\{\s-]+\\|{.*?}\\)" 1 'font-lock-function-name-face)
       ;; highlight predefined HM variables
       (list (concat "\\(\${?\\|\\<\\(set\\|global\\|variable\\)\\s-+\\)"
                     tcl-hm-variables-regexp)
             3 'font-lock-type-face)
       ;; highlight extra variables
       (list (concat "\\(\${?\\|\\<\\(set\\|global\\|variable\\)\\s-+\\)"
                     tcl-hm-extra-variables-regexp)
             3 'font-lock-warning-face)
       ;; highlight Tcl variables
       '("\\(\${?\\|\\<\\(set\\|global\\|variable\\)\\s-+\\)\\(\\w+\\)"
         3 'font-lock-variable-name-face)
       ;; highlight HM command options
       '("\\s-\\(-[a-z][_0-9a-z]*\\)\\>" 1 'font-lock-warning-face)
       ;; highlight Tcl keywords
       (list tcl-hm-keywords-regexp 1 'font-lock-keyword-face)
       ;; highlight extra commands
       (list tcl-hm-extra-commands-regexp 1 'font-lock-hm-face)
       ;; highlight predefined HM attributes
       (list tcl-hm-attributes-regexp 1 'font-lock-constant-face)
       ))

;; Why? Because font-lock support keywords less than 1000, we must split the long list into small sublists.
;; (font-lock-add-keywords 'tcl-mode (list (list tcl-hm-commands-regexp 1 'font-lock-keyword-face)))
(defun tcl-hm-add-keywords-hm-commands ()
  "Highlight all the commands defined in the tcl_hm.keywords."
  (unless tcl-hm-init-flag
    (setq tcl-hm-init-flag t)
    (with-temp-buffer
    (insert-file-contents-literally tcl-hm-keywords-file-name)
    (let ((tcl-hm-all-commands-list (split-string (buffer-string)))
          (tcl-hm-commands-length-limit nil)
          (tcl-hm-commands-regexp nil)
          (length-limit 901))
      (while tcl-hm-all-commands-list
        (setq tcl-hm-commands-length-limit
              (subseq tcl-hm-all-commands-list 0 (- length-limit 1)))
        (setq tcl-hm-all-commands-list
              (subseq tcl-hm-all-commands-list length-limit))
        (setq tcl-hm-commands-regexp
              (concat "\\<\\("
                      (regexp-opt
                       (append
                        tcl-hm-commands-length-limit
                        (unless tcl-hm-extra-highlight
                          tcl-hm-extra-commands))) "\\)\\>"))
        (font-lock-add-keywords
         'tcl-mode
         (list (list tcl-hm-commands-regexp 1 'font-lock-hm-face))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Electrification

(defun tcl-hm-electric-tab (&optional prefix-arg)
  "If preceeding character is part of a word or a paren then hippie-expand,
else if right of non whitespace on line then tab-to-tab-stop,
else if last command was a tab or return then dedent one step or if a comment
toggle between normal indent and inline comment indent,
else indent `correctly'.
If `tcl-hm-intelligent-tab' is nil, always indent line."
  (interactive "*P")
  (if tcl-hm-intelligent-tab
      (progn
        (cond
         ;; indent region if region is active
         ((and (not (featurep 'xemacs)) transient-mark-mode mark-active)
          (indent-region (region-beginning) (region-end) nil))
         ;; expand word
         ((memq (char-syntax (preceding-char)) '(?w ?_))
          (let ((case-fold-search t)
                (case-replace nil)
                (current-syntax-table (syntax-table))
                (hippie-expand-only-buffers
                 (or (and (boundp 'hippie-expand-only-buffers)
                          hippie-expand-only-buffers)
                     '(tcl-hm-mode))))
            (set-syntax-table tcl-hm-he-syntax-table)
            (tcl-hm-expand-abbrev prefix-arg)
            (set-syntax-table current-syntax-table)))
         ;; insert tab
         ((> (current-column) (current-indentation))
          (tab-to-tab-stop))
         ;; dedent
         ((and (eq last-command 'tcl-hm-electric-tab)
               (/= 0 (current-indentation)))
          (backward-delete-char-untabify tcl-indent-level nil))
         ;; indent
         (t (indent-according-to-mode)))
        (setq this-command 'tcl-hm-electric-tab))
    (indent-according-to-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hippie expand customization (for expansion of HM commands)

(defun tcl-hm-list-to-alist (list)
  "Convert list to alist."
  (let (alist)
    (while list
      (setq alist (cons (list (car list)) alist))
      (setq list (cdr list)))
    (nreverse alist)))

(defvar tcl-hm-commands-list
  (sort (append tcl-hm-keywords
                tcl-hm-commands
                (copy-sequence tcl-hm-extra-commands)
                tcl-hm-attributes
                (copy-sequence tcl-hm-extra-attributes)) 'string<)
  "List of Tcl keywords and HM commands.")

(defvar tcl-hm-variables-list
  (sort (append tcl-hm-variables
                (copy-sequence tcl-hm-extra-variables)) 'string<)
  "List of HM variables.")

(defvar tcl-hm-commands-alist (tcl-hm-list-to-alist tcl-hm-commands-list)
  "Alist of `tcl-hm-commands-list'.")

(defvar tcl-hm-variables-alist (tcl-hm-list-to-alist tcl-hm-variables-list)
  "Alist of `tcl-hm-variables-list'.")

(defvar tcl-hm-abbrev-list)

(defvar tcl-hm-abbrev-alist)

(eval-when-compile (require 'hippie-exp))

(defun tcl-hm-try-expand-abbrev (old)
  "Try expanding abbreviations from `tcl-hm-commands-list' and
`tcl-hm-variables-list'."
  (let ((current-syntax-table (syntax-table))
        common)
    (unless old
      ;; find expansion string
      (he-init-string (he-dabbrev-beg) (point))
      ;; if preceded by "$" or "set " expand variables
      (if (or (save-excursion (skip-syntax-backward "w")
                              (= (preceding-char) ?$))
              (save-excursion (skip-syntax-backward "w")
                              (skip-syntax-backward " ")
                              (skip-syntax-backward "w")
                              (looking-at "\\<set\\>")))
          (setq tcl-hm-abbrev-list tcl-hm-variables-list
                tcl-hm-abbrev-alist tcl-hm-variables-alist)
        ;; else expand commands
        (setq tcl-hm-abbrev-list tcl-hm-commands-list
              tcl-hm-abbrev-alist tcl-hm-commands-alist))
      ;; find common substring
      (setq common (try-completion he-search-string tcl-hm-abbrev-alist))
      ;; determine expansion list
      (setq he-expand-list
            (let ((abbrev-list tcl-hm-abbrev-list)
                  (sel-abbrev-list (when (and common (not (eq common t)))
                                     (list common))))
              (while abbrev-list
                (when (string-match
                       (concat "^" he-search-string) (car abbrev-list))
                  (setq sel-abbrev-list
                        (cons (car abbrev-list) sel-abbrev-list)))
                (setq abbrev-list (cdr abbrev-list)))
              (nreverse sel-abbrev-list))))
    ;; find next in expansion list
    (while (and he-expand-list
                (he-string-member (car he-expand-list) he-tried-table t))
      (setq he-expand-list (cdr he-expand-list)))
    ;; show expansion list in minibuffer
    (when (and common (> (length he-expand-list) 2))
      (message "%s" (cdr he-expand-list)))
    ;; expand
    (if (null he-expand-list)
        (progn (when old (he-reset-string))
               nil)
      (he-substitute-string (car he-expand-list) t)
      (setq he-expand-list (cdr he-expand-list))
      t)))

;; function for expanding abbrevs and dabbrevs
(defun tcl-hm-expand-abbrev (arg))
(fset 'tcl-hm-expand-abbrev (make-hippie-expand-function
                             '(try-expand-dabbrev
                               try-expand-dabbrev-all-buffers
                               tcl-hm-try-expand-abbrev)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comments

(defun tcl-hm-comment-uncomment-region (beg end &optional arg)
  "Comment region if not commented, uncomment region if already commented."
  (interactive "r\nP")
  (goto-char beg)
  (if (looking-at (regexp-quote comment-start))
      (comment-region beg end '(4))
    (comment-region beg end)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tcl-hm-doc-mode ()
  "Display Tcl HM Mode documentation in *Help* buffer."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ "Tcl HM Mode:\n")
    (princ (documentation 'tcl-hm-mode))
    (unless (string-match "XEmacs" emacs-version)
      (help-setup-xref (list #'tcl-hm-doc-mode) (interactive-p)))
    (save-excursion
      (set-buffer standard-output)
      (help-mode))
    (print-help-return-message)))

;;
;; Help-related code.
;;

(defvar tcl-hm-help-saved-dirs nil
  "Saved help directories.
If `tcl-hm-help-directory-list' changes, this allows `tcl-hm-help-on-word'
to update the alist.")

(defvar tcl-hm-help-directory-list nil
  "Users define help directories.
If `tcl-hm-help-directory-list' changes, this allows `tcl-hm-help-on-word'
to update the alist.")

(defvar tcl-hm-help-alist nil
  "Alist with command names as keys and filenames as values.")

(defun tcl-hm-get-command (html-filename)
  "Get the converted name of HTML-FILENAME _name.html to *name."
  (let ((new-name (file-name-base html-filename)))
    (setq new-name (replace-regexp-in-string "\\(^_\\)" "\*" new-name))))

(defun tcl-hm-files-alist (dir &optional alist)
  "Recursively add all pairs (FILE . PATH) under DIR to ALIST."
  (dolist (file (directory-files dir t) alist)
    (let ((filename (file-name-nondirectory file)))
      (cond
       ((not (file-directory-p file))
        (if (not (string= (subseq filename -5 -5) ")"))
            (push (cons (tcl-hm-get-command filename) file) alist)))
       ((member filename '("." "..")))
       (t (setq alist (tcl-hm-files-alist file alist)))))))

(defun tcl-hm-help-snarf-commands (dirlist)
  "Return alist of commands and filenames."
  (let ((alist nil))
    (dolist (dir dirlist alist)
      (when (file-directory-p dir)
        (setq alist (tcl-hm-files-alist dir alist))))))

(defun tcl-hm-reread-help-files ()
  "Set up to re-read files, and then do it."
  (interactive)
  (message "Building Tcl help file index...")
  (setq tcl-hm-help-saved-dirs tcl-hm-help-directory-list)
  (setq tcl-hm-help-alist (tcl-hm-help-snarf-commands tcl-hm-help-directory-list))
  (message "Building Tcl help file index...done"))

(defun tcl-hm-word-no-props ()
  "Like `current-word', but strips properties."
  (let ((word (current-word)))
    (set-text-properties 0 (length word) nil word)
    word))

(defun tcl-hm-current-word (flag)
  "Return current command word, or nil.
If FLAG is nil, just uses `current-word'.
Otherwise scans backward for most likely Tcl command word."
  (if (and flag
           (derived-mode-p 'tcl-mode 'inferior-tcl-mode))
      (condition-case nil
          (save-excursion
            ;; Look backward for first word actually in alist.
            (if (bobp)
                ()
              (while (and (not (bobp))
                          (not (tcl-real-command-p)))
                (backward-sexp)))
            (if (assoc (tcl-hm-word-no-props) tcl-hm-help-alist)
                (tcl-hm-word-no-props)))
        (error nil))
    (tcl-hm-word-no-props)))

(defun tcl-help-on-hm-word (command &optional arg)
  "Get help on Tcl command.  Default is word at point.
Prefix argument means invert sense of `tcl-use-smart-word-finder'."
  (interactive
   (list
    (progn
      (if (not (equal tcl-hm-help-directory-list tcl-hm-help-saved-dirs))
          (tcl-hm-reread-help-files))
      (let ((word (tcl-hm-current-word
                   (if current-prefix-arg
                       (not tcl-use-smart-word-finder)
                     tcl-use-smart-word-finder))))
        (completing-read
         (if (or (null word) (string= word ""))
             "Help on Tcl command: "
           (format "Help on Tcl command (default %s): " word))
         tcl-hm-help-alist nil t nil nil word)))
    current-prefix-arg))
  (if (not (equal tcl-hm-help-directory-list tcl-hm-help-saved-dirs))
      (tcl-hm-reread-help-files))
  (if (string= command "")
      (setq command (tcl-hm-current-word
                     (if arg
                         (not tcl-use-smart-word-finder)
                       tcl-use-smart-word-finder))))
  (let* ((cell (assoc command tcl-hm-help-alist))
         (file (and cell (cdr cell))))
    (if file
        (progn
          ;; Use the w3m to read the html files.
          (if (= (count-windows) 1)
              (split-window-horizontally))
          (print (current-buffer))
          (unless (eq (current-buffer) (get-buffer "*w3m*"))
            (other-window 1))
          (w3m-goto-url (concat "file://" file))
          )
      (if (string= command "")
          (insert "Magical Pig!")
        (insert "Tcl command " command " not in help\n")))
    (set-buffer-modified-p nil)
    ))

(defun tcl-hm-get-syntax-from-html (command &optional arg)
  "Get help on Tcl command.  Default is word at point.
Prefix argument means invert sense of `tcl-use-smart-word-finder'."
  (let* ((cell (assoc command tcl-hm-help-alist))
         (file (and cell (cdr cell)))
         (syntax nil))
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents-literally file)
          (beginning-of-buffer)
          (search-forward ">Syntax<" nil t 1)
          (next-line)
          (goto-char (line-end-position))
          (let ((begin (1+ (search-backward ">" nil t 3)))
                (end nil))
            (search-forward "<" nil t 1)
            (setq end (point))
            (setq syntax
                  (concat command
                          " "
                          (replace-regexp-in-string
                           "&quot;" "\""
                           (substring (buffer-string) (- begin 1) (- end 2)))))
            )
          ))
    syntax
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode definition

(defvar tcl-hm-mode nil
  "Tcl HM Mode.")

;; add to mode line
(add-to-list 'minor-mode-alist '(tcl-hm-mode " HM"))

(defun tcl-hm-mode ()
  "Extension to Tcl mode for editing HyperMesh (HM) Tcl scripts.

Features:
---------

  WORD/COMMAND COMPLETION:  Typing `TAB' after a (not completed) word
    completes the word according to the following rules:
    - Searches the current buffer for words that start alike and inserts the
      first word found.  Re-typing `TAB' toggles through alternative word
      completions.
    - Searches other open buffers for words that start alike and does the
      above.
    - Searches for predefined Tcl/HM commands, variables and attributes that
      start alike.  If only one completion is found it is inserted.  If
      multiple completions are found, the common substring they all start
      with is inserted and the list of found completions is shown in the
      minibuffer.  Re-typing `TAB' inserts the first one and then toggles
      through alternative completions.

    Typing `TAB' after a non-word character inserts a tabulator stop (if not
    at the beginning of a line).

    This functionality can be turned off and the default `TAB' behavior
    restored using custom option `tcl-hm-intelligent-tab'.

  HIGHLIGHTING (fontification):  Tcl/HM commands and their options, user
    variables, predefined variables and predefined attributes are additionally
    using different colors.

    Additional HM commands, variables and attributes can be added for
    highlighting and word completion using custom options
    `tcl-hm-extra-commands', `tcl-hm-extra-variables' and
    `tcl-hm-extra-attributes' (e.g. for hidden commands or variables).  These
    can be highlighted with a special warning color by setting custom option
    `tcl-hm-extra-highlight' to t.

  COMMENTS:  `C-c C-c' comments out a region if not commented out, and
    uncomments a region if already commented out.

  HM VERSION:  2014.06.


Installation:
-------------

Find information about installation and ways to automatically invoke Tcl HM
Mode for certain files in the header of this source file (`tcl-hm-mode.el').


Maintenance:
------------

Feel free to send bug reports, questions and enhancement requests to
<> (only for Tcl HM Mode, not for Tcl mode).


Key bindings:
-------------

\\{tcl-hm-mode-map}"
  (interactive)
  ;; load tcl-mode if not already active
  (unless (eq major-mode 'tcl-mode) (tcl-mode))
  ;; toggle mode
  (set (make-local-variable 'tcl-hm-mode) (not tcl-hm-mode))
  ;; set keymap
  (use-local-map (if tcl-hm-mode tcl-hm-mode-map tcl-mode-map))
  ;; set local variables
  (setq comment-padding " ")
  ;; update fontification
  (setq font-lock-defaults
        (if tcl-hm-mode
            ;; '(tcl-hm-font-lock-keywords)
            '(tcl-font-lock-keywords)
          '(tcl-font-lock-keywords)))
  ;; miscellaneous
  (set (make-local-variable 'hippie-expand-dabbrev-as-symbol) nil)
  (tcl-hm-add-keywords-hm-commands)
  (when tcl-hm-mode
    (message "Tcl HM Mode %s.  Type C-c C-h for documentation." tcl-hm-version)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'tcl-hm-mode)

;;; tcl-hm-mode.el ends here
