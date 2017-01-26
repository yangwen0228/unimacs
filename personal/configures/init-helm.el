;;; init-helm.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package helm
  :bind (("M-x"     . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b"   . helm-multi-files)
         ("C-c i"   . helm-imenu))
  :config
  (require 'helm-config)
  (helm-mode 1)                       ;turn helm-mode on, don't turn ido-mode on

  (setq helm-autoresize-mode t
        helm-scroll-amount   1
        helm-ff-newfile-prompt-p nil
        helm-ff-skip-boring-files t)
  ;; Overwrite: don't need "." and "..".
  (defun helm-ff-directory-files (directory &optional full)
    "List contents of DIRECTORY.
Argument FULL mean absolute path.
It is same as `directory-files' but always returns the
dotted filename '.' and '..' even on root directories in Windows
systems."
    (setq directory (file-name-as-directory
                     (expand-file-name directory)))
    (let* (file-error
           (ls   (condition-case err
                     (directory-files
                      directory full directory-files-no-dot-files-regexp)
                   ;; Handle file-error from here for Windows
                   ;; because predicates like `file-readable-p' and friends
                   ;; seem broken on emacs for Windows systems (always returns t).
                   ;; This should never be called on GNU/Linux/Unix
                   ;; as the error is properly intercepted in
                   ;; `helm-find-files-get-candidates' by `file-readable-p'.
                   (file-error
                    (prog1
                        (list (format "%s:%s"
                                      (car err)
                                      (mapconcat 'identity (cdr err) " ")))
                      (setq file-error t)))))
           (dot  (concat directory "."))
           (dot2 (concat directory "..")))
      (puthash directory (+ (length ls) 2) helm-ff--directory-files-hash)
      ;; (append (and (not file-error) (list dot dot2)) ls) ; origin
      (append (and (not file-error) (list dot)) ls)))

  (setq helm-completing-read-handlers-alist
        '((execute-extended-command . helm-completing-read-symbols)
          (describe-function        . helm-completing-read-symbols)
          (describe-variable        . helm-completing-read-symbols)
          (debug-on-entry           . helm-completing-read-symbols)
          (find-function            . helm-completing-read-symbols)
          (find-tag                 . helm-completing-read-with-cands-in-buffer)
          (find-file                . helm-completing-read-default-1)
          (ido-find-file            . helm-completing-read-default-1)
          (ido-kill-buffer          . helm-completing-read-default-1)
          (mml-attach-file          . helm-completing-read-default-1)
          (read-file-name           . helm-completing-read-default-1)
          (read-directory-name      . helm-completing-read-default-1)
          (yas-compile-directory    . helm-completing-read-default-1)
          (ffap-alternate-file      . helm-completing-read-default-1)
          (tmm-menubar                . nil)
          (dired-do-copy              . nil)
          (dired-do-rename            . nil)
          (dired-create-directory     . nil)
          (ido-edit-input             . nil)
          (minibuffer-completion-help . nil)
          (minibuffer-complete        . nil)
          (rgrep                      . nil)
          (w3m-goto-url               . nil)
          ))

  ;; pinyin filter
  (setq helm-pinyin-search-p t)
  (when helm-pinyin-search-p
    (require 'pinyin-search))
  (defsubst helm--mapconcat-pinyin-pattern (pattern)
    "Transform string PATTERN in regexp for further fuzzy matching.
e.g helm.el$
    => \"[^h]*h[^e]*e[^l]*l[^m]*m[^.]*[.][^e]*e[^l]*l$\"
    ^helm.el$
    => \"helm[.]el$\"."
    (let ((ls (split-string-and-unquote pattern "")))
      (if (string= "^" (car ls))
          ;; Exact match.
          (mapconcat (lambda (c)
                       (if (and (string= c "$")
                                (string-match "$\\'" pattern))
                           c (regexp-quote c)))
                     (cdr ls) "")
        ;; Fuzzy match.
        (mapconcat (lambda (c)
                     (if (and (string= c "$")
                              (string-match "$\\'" pattern))
                         c (let ((pinyin-pattern (pinyinlib-build-regexp-string c)))
                             (if (< (length pinyin-pattern) 3)
                                 c
                               (format "[^%s]*%s" (substring pinyin-pattern 1 -1) pinyin-pattern)))))
                   ls ""))))

  (defun helm-find-files-get-candidates (&optional require-match)
    "Create candidate list for `helm-source-find-files'."
    (let* ((path          (helm-ff-set-pattern helm-pattern))
           (dir-p         (file-accessible-directory-p path))
           basedir
           invalid-basedir
           non-essential
           (tramp-verbose helm-tramp-verbose)) ; No tramp message when 0.
      ;; Tramp check if path is valid without waiting a valid
      ;; connection and may send a file-error.
      (setq helm--ignore-errors (file-remote-p path))
      (set-text-properties 0 (length path) nil path)
      ;; Issue #118 allow creation of newdir+newfile.
      (unless (or
               ;; A tramp file name not completed.
               (string= path "Invalid tramp file name")
               ;; An empty pattern
               (string= path "")
               (and (string-match-p ":\\'" path)
                    (helm-ff-tramp-postfixed-p path))
               ;; Check if base directory of PATH is valid.
               (helm-aif (file-name-directory path)
                   ;; If PATH is a valid directory IT=PATH,
                   ;; else IT=basedir of PATH.
                   (file-directory-p it)))
        ;; BASEDIR is invalid, that's mean user is starting
        ;; to write a non--existing path in minibuffer
        ;; probably to create a 'new_dir' or a 'new_dir+new_file'.
        (setq invalid-basedir t))
      ;; Don't set now `helm-pattern' if `path' == "Invalid tramp file name"
      ;; like that the actual value (e.g /ssh:) is passed to
      ;; `helm-ff-tramp-hostnames'.
      (unless (or (string= path "Invalid tramp file name")
                  invalid-basedir)      ; Leave  helm-pattern unchanged.
        (setq helm-ff-auto-update-flag  ; [1]
              ;; Unless auto update is disabled at startup or
              ;; interactively, start auto updating only at third char.
              (unless (or (null helm-ff-auto-update-initial-value)
                          (null helm-ff--auto-update-state)
                          ;; But don't enable auto update when
                          ;; deleting backward.
                          helm-ff--deleting-char-backward
                          (and dir-p (not (string-match-p "/\\'" path))))
                (or (>= (length (helm-basename path)) 3) dir-p)))
        ;; At this point the tramp connection is triggered.
        (setq helm-pattern (helm-ff--transform-pattern-for-completion path))
        ;; (print (concat "8 " helm-pattern))
        ;; This have to be set after [1] to allow deleting char backward.
        (setq basedir (expand-file-name
                       (if (and dir-p helm-ff-auto-update-flag)
                           ;; Add the final "/" to path
                           ;; when `helm-ff-auto-update-flag' is enabled.
                           (file-name-as-directory path)
                         (if (string= path "")
                             "/" (file-name-directory path)))))
        (setq helm-ff-default-directory
              (if (string= helm-pattern "")
                  (expand-file-name "/")  ; Expand to "/" or "c:/"
                ;; If path is an url *default-directory have to be nil.
                (unless (or (string-match helm-ff-url-regexp path)
                            (and ffap-url-regexp
                                 (string-match ffap-url-regexp path)))
                  basedir))))
      (when (and (string-match ":\\'" path)
                 (file-remote-p basedir nil t))
        (setq helm-pattern basedir))
      (cond ((string= path "Invalid tramp file name")
             (or (helm-ff-tramp-hostnames) ; Hostnames completion.
                 (prog2
                     ;; `helm-pattern' have not been modified yet.
                     ;; Set it here to the value of `path' that should be now
                     ;; "Invalid tramp file name" and set the candidates list
                     ;; to ("Invalid tramp file name") to make `helm-pattern'
                     ;; match single candidate "Invalid tramp file name".
                     (setq helm-pattern path)
                     ;; "Invalid tramp file name" is now printed
                     ;; in `helm-buffer'.
                     (list path))))
            ((or (and (file-regular-p path)
                      (eq last-repeatable-command 'helm-execute-persistent-action))
                 ;; `ffap-url-regexp' don't match until url is complete.
                 (string-match helm-ff-url-regexp path)
                 invalid-basedir
                 (and (not (file-exists-p path)) (string-match "/$" path))
                 (and ffap-url-regexp (string-match ffap-url-regexp path)))
             (list path))
            ((string= path "") (helm-ff-directory-files "/" t))
            ;; Check here if directory is accessible (not working on Windows).
            ((and (file-directory-p path) (not (file-readable-p path)))
             (list (format "file-error: Opening directory permission denied `%s'" path)))
            ;; A fast expansion of PATH is made only if `helm-ff-auto-update-flag'
            ;; is enabled.
            ((and dir-p helm-ff-auto-update-flag)
             (helm-ff-directory-files path t))
            (t (append (unless (or require-match
                                   ;; When `helm-ff-auto-update-flag' has been
                                   ;; disabled, whe don't want PATH to be added on top
                                   ;; if it is a directory.
                                   dir-p)
                         (list path))
                       (helm-ff-directory-files basedir t))))))

  (defun helm--fuzzy-match-maybe-set-pattern ()
    ;; Computing helm-pattern with helm--mapconcat-pattern
    ;; is costly, so cache it once time for all and reuse it
    ;; until pattern change.
    (when helm--in-fuzzy
      (let ((fun (if (string-match "\\`\\^" helm-pattern)
                     #'identity
                   #'helm--mapconcat-pinyin-pattern)))
        (clrhash helm--fuzzy-regexp-cache)
        ;; FIXME: Splitted part are not handled here,
        ;; I must compute them in `helm-search-match-part'
        ;; when negation and in-buffer are used.
        (if (string-match "\\`!" helm-pattern)
            (puthash 'helm-pattern
                     (if (> (length helm-pattern) 1)
                         (list (funcall fun (substring helm-pattern 1 2))
                               (funcall fun (substring helm-pattern 1)))
                       '("" ""))
                     helm--fuzzy-regexp-cache)
          (puthash 'helm-pattern
                   (if (> (length helm-pattern) 0)
                       (list (funcall fun (substring helm-pattern 0 1))
                             (funcall fun helm-pattern))
                     '("" ""))
                   helm--fuzzy-regexp-cache)))))

  (defun helm-ff--transform-pattern-for-completion (pattern)
    "Maybe return PATTERN with it's basename modified as a regexp.
This happen only when `helm-ff-fuzzy-matching' is enabled.
This provide a similar behavior as `ido-enable-flex-matching'.
See also `helm--mapconcat-pinyin-pattern'
If PATTERN is an url returns it unmodified.
When PATTERN contain a space fallback to multi-match.
If basename contain one or more space fallback to multi-match.
If PATTERN is a valid directory name,return PATTERN unchanged."
    ;; handle bad filenames containing a backslash.
    (setq pattern (helm-ff-handle-backslash pattern))
    (let ((bn      (helm-basename pattern))
          (bd      (or (helm-basedir pattern) ""))
          ;; Trigger tramp connection with file-directory-p.
          (dir-p   (file-directory-p pattern))
          (tramp-p (cl-loop for (m . f) in tramp-methods
                            thereis (string-match m pattern))))
      ;; Always regexp-quote base directory name to handle
      ;; crap dirnames such e.g bookmark+
      (cond
       ((or (and dir-p tramp-p (string-match ":\\'" pattern))
            (string= pattern "")
            (and dir-p (<= (length bn) 2))
            ;; Fix Issue #541 when BD have a subdir similar
            ;; to BN, don't switch to match plugin
            ;; which will match both.
            (and dir-p (string-match (regexp-quote bn) bd)))
        ;; Use full PATTERN on e.g "/ssh:host:".
        (regexp-quote pattern))
       ;; Prefixing BN with a space call multi-match completion.
       ;; This allow showing all files/dirs matching BN (Issue #518).
       ;; FIXME: some multi-match methods may not work here.
       (dir-p (concat (regexp-quote bd) " " (regexp-quote bn)))
       ((or (not (helm-ff-fuzzy-matching-p))
            (string-match "\\s-" bn))    ; Fall back to multi-match.
        (concat (regexp-quote bd) bn))
       ((or (string-match "[*][.]?.*" bn) ; Allow entering wilcard.
            (string-match "/$" pattern)     ; Allow mkdir.
            (string-match helm-ff-url-regexp pattern)
            (and (string= helm-ff-default-directory "/") tramp-p))
        ;; Don't treat wildcards ("*") as regexp char.
        ;; (e.g ./foo/*.el => ./foo/[*].el)
        (concat (regexp-quote bd)
                (replace-regexp-in-string "[*]" "[*]" bn)))
       (t (concat (regexp-quote bd)
                  (if (>= (length bn) 2) ; wait 2nd char before concating.
                      (progn
                        ;; (print (helm--mapconcat-pinyin-pattern bn))
                        (helm--mapconcat-pinyin-pattern bn))
                    (concat ".*" (regexp-quote bn)))))
       ;; (t (concat (regexp-quote bd)
       ;;             (let ((eng-pattern (if (>= (length bn) 2) ; wait 2nd char before concating.
       ;;                                    (helm--mapconcat-pinyin-pattern bn)
       ;;                                  (concat ".*" (regexp-quote bn)))))
       ;;               (if helm-pinyin-search-p
       ;;                   (concat "\\(" eng-pattern "\\)" "\\|" "\\(" ".*" (pinyin-search--pinyin-to-regexp bn) ".*" "\\)")
       ;;                 eng-pattern))))
       )))

  (cl-defun helm-mm-3-match (str &optional (pattern helm-pattern))
    "Check if PATTERN match STR.
When PATTERN contain a space, it is splitted and matching is done
with the several resulting regexps against STR.
e.g \"bar foo\" will match \"foobar\" and \"barfoo\".
Argument PATTERN, a string, is transformed in a list of
cons cell with `helm-mm-3-get-patterns' if it contain a space.
e.g \"foo bar\"=>((identity . \"foo\") (identity . \"bar\")).
Then each predicate of cons cell(s) is called with regexp of same
cons cell against STR (a candidate).
i.e (identity (string-match \"foo\" \"foo bar\")) => t."
    (let ((pat (helm-mm-3-get-patterns pattern)))
      (let ((source-name (assoc-default 'name (helm-get-current-source))))
        ;; (print (concat "8 " source-name))
        (if (string= source-name "Recentf")
            (cl-loop for (predicate . regexp) in pat
                     always (funcall predicate
                                     (condition-case _err
                                         ;; FIXME: Probably do nothing when
                                         ;; using fuzzy leaving the job
                                         ;; to the fuzzy fn.
                                         (string-match
                                          (concat "\\(" regexp "\\)\\|\\(" (pinyin-search--pinyin-to-regexp regexp) "\\)") str)
                                       (invalid-regexp nil))))
          (cl-loop for (predicate . regexp) in pat
                   always (funcall predicate
                                   (condition-case _err
                                       ;; FIXME: Probably do nothing when
                                       ;; using fuzzy leaving the job
                                       ;; to the fuzzy fn.
                                       (string-match regexp str)
                                     (invalid-regexp nil))))
          ))))

  (defun helm-my-test (candidate)
    (print candidate))
  (setq helm-find-files-actions (append helm-find-files-actions '(("my-test" . helm-my-test))))

  :diminish (helm-mode)
  )

(provide 'init-helm)
;;; init-helm.el ends here
