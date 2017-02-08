;;; tcl-hm-eldoc.el --- tcl-hm-mode support for eldoc-mode -*- lexical-binding: t -*-
;;
;; Copyright (C) 2017 Wen Yang
;;
;; Author: Wen Yang <github.com/yangwen0228>
;; Version: 0.1
;; Keywords: tcl-hm-mode eldoc
;; URL: https://github.com/yangwen0228
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (tcl-hm "0.1"))
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This implements eldoc support in tcl-hm-mode.  eldoc is a built-in
;; Emacs mode for displaying documentation about a symbol or function
;; call at point in the message buffer (see `eldoc-mode').
;;
;; To use:
;;
;; Enable the minor mode `tcl-hm-eldoc', as well as `eldoc-mode'. For
;; an example, place point on top of a symbol, or inside a function
;; call.
;;
;; It is easiest to add `tcl-hm-eldoc' to `tcl-hm-mode-hook', if you
;; already have `tcl-hm-mode' set up.
;;
;; Notes:
;;
;; - It is based on tcl-hm-mode and modified from irony-eldoc.
;;
;;; Code:

(require 'tcl-hm-mode)
(require 'thingatpt)
(require 'cl-lib)
(require 'eldoc)

;; {{{ Customizations

(defgroup tcl-hm-eldoc nil
  "eldoc support in `tcl-hm-mode'.

eldoc is a built-in emacs mode for displaying documentation about
a symbol or function call at point in the message buffer (see
`eldoc-mode')."
  :group 'tcl-hm-eldoc)

(defcustom tcl-hm-eldoc-strip-underscores
  t
  "In a type, strip leading underscores from all identifiers.

Many common types, especially in the standard library in c++,
have these underscores, which carry no extra information."
  :group 'tcl-hm-eldoc
  :type 'boolean)

;; }}}
;; {{{ Utilities

(defun tcl-hm-eldoc--strip-underscores (string)
  "Strip leading underscores from all identifiers in STRING.

It also prettifies the string by replacing things like \"::\"
with their Unicode equivalents.

Has no effect if `tcl-hm-eldoc-strip-underscores' is non-nil."
  (if (or (not string) (not tcl-hm-eldoc-strip-underscores))
      string
    (let ((new-string string)
          (regexps '(("\\_<_+" . "") ("::" . "∷"))))
      (dolist (r regexps)
        (setq new-string
              (replace-regexp-in-string (car r) (cdr r) new-string)))
      new-string)))

(defvar tcl-hm-eldoc--ignore-symbol-regex
  (rx (or
       (and (1+ digit) (opt "e" (opt (1+ digit))))
       (or ;; Taken from `cc-langs'
        "bool" "char" "wchar_t" "short" "int" "long" "signed" "unsigned"
        "float" "double" "void" "_Bool" "_Complex" "_Imaginary"
        "id" "Class" "SEL" "IMP" "BOOL" "struct" "union" "enum"
        "class" "typename" "const" "restrict" "volatile" "throw"
        "@interface" "@implementation" "@protocol"
        "namespace" "extern" "auto" "extern" "inline" "register" "static"
        "explicit" "friend" "mutable" "template" "using" "virtual"
        "auto" "bycopy" "byref" "extern" "in" "inout" "oneway" "out" "static"
        "@class" "@end" "@defs"
        "__attribute__" "__declspec"
        "private" "protected" "public"
        "@private" "@protected" "@public"
        "struct" "union" "enum" "typedef"
        "class" "struct" "union" "enum" "typedef"
        "operator" "@class"
        "template"
        "do" "else" "try" "@finally" "@try"
        "for" "if" "switch" "while" "catch" "@catch" "@synchronized"
        "break" "continue" "goto" "return" "@throw"
        "asm" "__asm__"
        "case" "default"
        "goto" "break" "continue"
        "NULL" "nullptr" "false" "true"
        "nil" "Nil" "YES" "NO" "NS_DURING" "NS_HANDLER" "NS_ENDHANDLER"
        "operator" "this" "super" "self")))
  "Regex for identifiers that tcl-hm-eldoc should ignore entirely.

This is primitive types, common types, common values (NULL, true,
false), various keywords that may appear sometimes but for which
there should be no documentation.")

(defun tcl-hm-eldoc--which-symbol ()
  "Return a symbol under point suitable for documentation."
  ;; Require that char-after should be word-/symbol-constituent
  (let (bounds thing)
    (when (and (let ((s (car (syntax-after (point)))))
                 (and s (or (= s 2) (= s 3)))) ; Note that s can be nil
               (setq bounds (bounds-of-thing-at-point 'symbol))
               (setq thing (buffer-substring-no-properties
                            (car bounds) (cdr bounds)))
               ;; Check thing is not a built-in type or something useless
               (not (string-match-p tcl-hm-eldoc--ignore-symbol-regex thing)))
      (list nil thing (car bounds) (cdr bounds)))))

(defun tcl-hm-eldoc--argindex (&optional pos open-paren close-paren)
  "Return the index of the argument at POS inside parentheses.

Returns cons pair '(argindex . argcount), with 0 <= argindex < argcount.

OPEN-PAREN and CLOSE-PAREN are assumed to be balanced parens with everything balanced inside them as well."
  (unless pos (setq pos (point)))
  (unless open-paren
    (save-excursion
      (backward-up-list)
      (setq open-paren (point))
      (forward-list)
      (setq close-paren (point))))
  (let ((argindex 0) (argcount 1))
    (save-excursion
      (goto-char (1+ open-paren))
      (while (< (point) close-paren)
        (skip-syntax-forward "w_-" close-paren)
        (while (and (< (point) close-paren)
                    ;; open paren of any kind
                    (= (car (syntax-after (point))) 4))
          ;; works for any balanced group, not just parens
          (forward-list))
        (when (= (char-after) ?,)
          (when (< (point) pos) (cl-incf argindex))
          (cl-incf argcount))
        (forward-char)))
    ;; Add the number of template arguments to argcount
    (save-excursion
      (goto-char open-paren)
      (when (= (char-before) ?>)
        (let (template-count open-template (close-template (point)))
          (backward-list)
          (setq open-template (point)
                template-count
                (cdr (tcl-hm-eldoc--argindex
                      (point) open-template close-template)))
          (setq argcount (+ argcount template-count)
                argindex (+ argindex template-count)))))
    (cons argindex argcount)))

(defun tcl-hm-eldoc--which-funcall ()
  "Return description of surrounding function call,

suitable for `tcl-hm-eldoc--which-thing'.  Throws an
error (scan-error) on any unrecognized syntax, so probably call
inside `condition-case'."
  (let (bounds thing (old-point (point)) open-paren close-paren)
    (save-excursion
      ;; the escape-strings argument is not present in 24.4
      ;; (backward-up-list nil t) ; escape strings
      ;; if inside a string, move out of the string first
      (let ((syntax (syntax-ppss))
            (point (point))
            (line-beg (line-beginning-position)))
        (when (nth 3 syntax) (goto-char (nth 8 syntax))))
      (re-search-backward "\\[" line-beg)
      (if (= point (point))
          (progn
            (beginning-of-line)
            (back-to-indentation)
            ))
      (when (and (= (char-after) #x5b)    ; open \[
               (setq open-paren (point)
                     close-paren (save-excursion (forward-list) (point))))
        (setq bounds (bounds-of-thing-at-point 'symbol)
              thing (buffer-substring-no-properties
                     (car bounds) (cdr bounds)))))
    (when thing
      (list (tcl-hm-eldoc--argindex old-point open-paren close-paren)
            thing (car bounds) (cdr bounds)))))

(defun tcl-hm-eldoc--which-thing (&optional force-funcall)
  "Return the buffer substring and its bounds for which doc should be shown.

If FORCE-FUNCALL is non-nil, look for the symbol at the head of
the surrounding function call, otherwise try to guess if that's
appropriate.

Returns nil if there is nothing suitable under point.

Returns a list of the form

  (arg-index thing-string thing-start thing-end)

where arg-index is nil if doc should be displayed for the symbol
at point, or (argindex . argcount) if it is for the function call
surrounding point."
  (let* ((ppss (syntax-ppss (point)))
         ;; Do nothing inside strings and comments
         (in-string (nth 3 ppss))
         (in-comment (nth 4 ppss)))
    (unless in-comment
      (or (and (not force-funcall)
               (not in-string)
               (tcl-hm-eldoc--which-symbol))
          (condition-case nil
              (tcl-hm-eldoc--which-funcall)
            (scan-error nil))))))

;; }}}
;; {{{ Making displayed strings

(defun tcl-hm-eldoc--show-symbol (prop)
  "Return docstring for a given symbol.

The symbol is specified by PROP, which is an object taken from
`tcl-hm-completion-candidates'."
  ;; Show documentation for a symbol.
  ;; variable of type T: "variable => T"
  ;; void function(...): "function(...)"
  ;; T function(...): "function(...) => T"
  (let* ((name (propertize (tcl-hm-completion-typed-text prop)
                           'face 'eldoc-highlight-function-argument))
         (result-type (tcl-hm-completion-type prop))
         (post-completion-data
          (cons (tcl-hm-completion-post-comp-str prop)
                (tcl-hm-completion-post-comp-placeholders prop)))
         (has-result-type (not (string= "" result-type)))
         (arglist (car post-completion-data))
         (has-arglist (not (string= "" arglist)))
         (docstring (tcl-hm-completion-brief prop))
         (has-docstring (not (string= "" docstring))))
    (unless (string= "" docstring)
      (setq docstring (propertize docstring 'face 'variable-pitch))
      (setq docstring (concat "; " docstring)))
    (tcl-hm-eldoc--strip-underscores
     (cond
      ;; Things like builtin types have nothing of interest.
      ((and (not has-arglist) (not has-result-type) (not has-docstring))
       nil)
      ((and (not has-arglist) has-result-type)
       (concat name " ⇒ " result-type docstring))
      (has-result-type
       (concat name arglist " ⇒ " result-type docstring))
      (t
       nil)))))

(defun tcl-hm-eldoc--show-funcall (arg-index arg-count prop)
  "Return docstring for a given function call.

ARG-INDEX and ARG-COUNT specify the index of function argument to
be highlighted, and PROP is an object from
`tcl-hm-completion--candidates'."
  ;; Show documentation inside a function call
  (let* ((name (tcl-hm-completion-typed-text prop))
         ;; FIXME The result type is "void" for constructors
         (result-type (tcl-hm-completion-type prop))
         (has-result-type (not (string= "" result-type)))
         (post-completion-data
          (cons (tcl-hm-completion-post-comp-str prop)
                (tcl-hm-completion-post-comp-placeholders prop)))
         (arglist (car post-completion-data))
         (has-arguments (not (string= "" arglist)))
         (docstring (tcl-hm-completion-brief prop))
         (has-docstring (not (string= "" docstring))))
    (when has-docstring
      (setq docstring (propertize docstring 'face 'variable-pitch))
      (setq docstring (concat "; " docstring)))
    (when (and has-arguments
               (>= (length post-completion-data)
                  (1+ (* 2 arg-count))))
      (let ((from (nth (+ 1 (* 2 arg-index)) post-completion-data))
            (to (nth (+ 2 (* 2 arg-index)) post-completion-data)))
        (setq arglist
              (concat (substring arglist 0 from)
                      (propertize (substring arglist from to)
                                  'face 'eldoc-highlight-function-argument)
                      (substring arglist to)))))
    (tcl-hm-eldoc--strip-underscores
     (if (or has-result-type has-docstring)
         (concat name arglist " ⇒ " result-type docstring)
       (concat name arglist)))))

;; }}}
;; {{{ eldoc support

(defun tcl-hm-eldoc--callback (thing &optional continuation)
  "Store found documentation in an overlay on THING,
for use by future calls to `tcl-hm-eldoc-documentation-function'.

THING is expected to be of the form

  (thing-string thing-start thing-end)

where the symbol between thing-start and thing-end should have
its documentation stored.

Once this is done, CONTINUATION will be called."
  ;; (message "tcl-hm-eldoc--callback %s: %d candidates" thing (length tcl-hm-completion--candidates))
  (let ((current-thing (buffer-substring-no-properties (nth 1 thing) (nth 2 thing)))
        (matches
         (cl-remove-if-not
          (lambda (x) (equal (car x) (car thing)))
          ;; FIXME This really should be (tcl-hm-completion-candidates)
          ;; but that function looks at (point) to see if completion
          ;; context is the same, so we use the internal tcl-hm-mode variable.
          tcl-hm-completion--candidates)))
    (when (equal current-thing (car thing))
      (let ((o (make-overlay (nth 1 thing) (nth 2 thing))))
        (overlay-put o 'category 'tcl-hm-eldoc)
        (overlay-put o 'tcl-hm-eldoc matches))
      (funcall continuation))))

(defun tcl-hm-eldoc-documentation-function_bak (&optional only-use-cached)
  "Support for eldoc in function `tcl-hm-mode'.

See `eldoc-documentation-function' for what this function is
supposed to do.

If ONLY-USE-CACHED is non-nil, only look at cached documentation."
  (let* ((in-string (nth 3 (syntax-ppss)))
         ;; If inside a string, look for a surrounding function call.
         (thing (tcl-hm-eldoc--which-thing in-string))
         ;; Previous lookups of documentation are stored in char property
         ;; 'tcl-hm-eldoc (which belongs to an overlay on top of the symbol).
         (props (and thing (get-char-property (nth 2 thing) 'tcl-hm-eldoc))))
    ;; (dolist (p props) (message "%s" (prin1-to-string p)))
    (cond
     ((not thing) nil)

     ;; Here each element of props is an object that came from
     ;; `tcl-hm-completion-candidates' that matches the symbol whose
     ;; information needs to be displayed.
     ((and props (not (car thing)))
      (let ((matching-docstrings
             (remove-if-not
              #'identity (mapcar #'tcl-hm-eldoc--show-symbol props))))
        (when matching-docstrings
          (mapconcat #'identity matching-docstrings ";; "))))

     ;; For a function call there will often be many different matches
     ;; in `tcl-hm-completion-candidates', so here we select all of
     ;; them that have the same number of arguments.
     ;; FIXME This doesn't distinguish between template and function arguments.
     (props
      (let* ((arg-index (caar thing))
             (arg-count (cdar thing))
             (matching-props
              ;; Matching function calls with the right number of arguments
              (remove-if-not
               (lambda (it) (= (length (nth 6 it)) (1+ (* 2 arg-count))))
               props))
             (docstring (mapconcat
                         (apply-partially
                          #'tcl-hm-eldoc--show-funcall arg-index arg-count)
                         matching-props
                         ";; ")))
        (unless (string= "" docstring) docstring)))

     ;; If there is no cached doc, a request is made, which may or may
     ;; not return immediately.
     ((not only-use-cached)
      (save-excursion
        (goto-char (nth 3 thing))
        (lexical-let ((callback-thing (cdr thing))
                      (async-flag nil)
                      (matches-available nil))
          ;; Sometimes the callback is called immediately, and
          ;; sometimes it is called later. Both cases need to be
          ;; handled properly.
          (tcl-hm-completion-candidates-async
           (lambda ()
             (tcl-hm-eldoc--callback
              callback-thing
              (lambda () (if async-flag
                             (eldoc-print-current-symbol-info)
                           (setq matches-available t))))))
          (setq async-flag t)
          (when matches-available
            (tcl-hm-eldoc-documentation-function t))))))))

(defun tcl-hm-eldoc-documentation-function (&optional only-use-cached)
  "Support for eldoc in function `tcl-hm-mode'.

See `eldoc-documentation-function' for what this function is
supposed to do.

If ONLY-USE-CACHED is non-nil, only look at cached documentation."
  (let* ((in-string (nth 3 (syntax-ppss)))
         ;; If inside a string, look for a surrounding function call.
         (thing (tcl-hm-eldoc--which-thing in-string))
         ;; Previous lookups of documentation are stored in char property
         ;; 'tcl-hm-eldoc (which belongs to an overlay on top of the symbol).
         (props (and thing (get-char-property (nth 2 thing) 'tcl-hm-eldoc))))
    ;; (dolist (p props) (message "%s" (prin1-to-string p)))
    (cond
     ((not thing) nil)

     ;; Here each element of props is an object that came from
     ;; `tcl-hm-completion-candidates' that matches the symbol whose
     ;; information needs to be displayed.
     ((and props (not (car thing)))
      (let ((matching-docstrings
             (remove-if-not
              #'identity (mapcar #'tcl-hm-eldoc--show-symbol props))))
        (when matching-docstrings
          (mapconcat #'identity matching-docstrings ";; "))))
     ((member (nth 1 thing) tcl-hm-commands)
      (tcl-hm-get-syntax-from-html (nth 1 thing)))
     )))
;; }}}
;; {{{ Minor mode

(defun tcl-hm-eldoc-reset ()
  "Reset information used by `tcl-hm-eldoc'.

Can be helpful is `tcl-hm-eldoc' starts displaying stale un-updated
information."
  (interactive)
  (remove-overlays (point-min) (point-max) 'category 'tcl-hm-eldoc))

;;;###autoload
(define-minor-mode tcl-hm-eldoc
  "Eldoc support in tcl-hm-mode.

eldoc is a built-in Emacs mode for displaying documentation about
a symbol or function call at point in the message buffer (see
`eldoc-mode').

To use:

- Enable the minor mode `tcl-hm-eldoc', as well as
  `eldoc-mode'. For an example, place point on top of a symbol,
  or inside a function call.

- It is easiest to add `tcl-hm-eldoc' to `tcl-hm-mode-hook', if you
  already have `tcl-hm-mode' set up.

Notes:

- Sometimes the information `tcl-hm-eldoc' uses can go out of
  date. In that case, try calling `tcl-hm-eldoc-reset'."
  :group 'tcl-hm-eldoc
  ;; FIXME This deletes documentation overlays not conservatively enough
  ;; There are more changes. that can make an overlay invalid.
  (let ((hook (lambda (o _beforep _start _end &optional _change-length)
                (delete-overlay o))))
    (put 'tcl-hm-eldoc 'modification-hooks (list hook))
    (put 'tcl-hm-eldoc 'insert-in-front-hooks (list hook))
    (put 'tcl-hm-eldoc 'insert-behind-hooks (list hook)))
  (cond
   (tcl-hm-eldoc
    (setq-local eldoc-documentation-function
                #'tcl-hm-eldoc-documentation-function)
    (unless eldoc-mode (eldoc-mode)))
   (t
    (when (eq eldoc-documentation-function
              #'tcl-hm-eldoc-documentation-function)
      (setq-local eldoc-documentation-function nil)))))

;; }}}

(provide 'tcl-hm-eldoc)
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; coding: utf-8-unix
;; End:
;;; tcl-hm-eldoc.el ends here
