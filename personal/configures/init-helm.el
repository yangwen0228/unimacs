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
  (helm-mode 1) ;turn helm-mode on, don't turn ido-mode on

  (use-package imenu :ensure nil
    ;; type name must be capital, like: Use-package. Cannot use use-package.
    :init
    (add-to-list 'lisp-imenu-generic-expression
                 (list "Use-package"
                       (concat
                        "^\\s-*(use-package\\s-+" ; definition
                        "\\([-A-Za-z0-9_:+*]+\\)" ; package name
                        )
                       1)))

  (setq helm-autoresize-mode t
        helm-scroll-amount   1
        helm-ff-newfile-prompt-p nil
        helm-ff-skip-boring-files t)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)   ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action)              ; list actions using C-z

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
          (yas-visit-snippet-file   . helm-completing-read-default-1)
          (ffap-alternate-file      . helm-completing-read-default-1)
          (tmm-menubar                . nil)
          (dired-do-copy              . helm-completing-read-default-1)
          (dired-do-rename            . helm-completing-read-default-1)
          (dired-create-directory     . helm-completing-read-default-1)
          (ido-edit-input             . nil)
          (minibuffer-completion-help . nil)
          (minibuffer-complete        . nil)
          (rgrep                      . nil)
          (w3m-goto-url               . nil)
          ))

  (defun helm-ff-retrieve-last-expanded ()
    "Overwrite the origin function. When go up one level, just select the . directory."
    (setq helm-ff-last-expanded nil))

  ;; pinyin filter
  (setq helm-pinyin-search-p t)
  (when helm-pinyin-search-p
    (use-package pinyin-search))
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
        (if (< (length ls) 8)           ; when string length is too big, too many Chinese chars, just use English then.
            (mapconcat (lambda (c)
                         (if (and (string= c "$")
                                  (string-match "$\\'" pattern))
                             c
                           (let ((pinyin-pattern (pinyinlib-build-regexp-string c)))
                             (if (< (length pinyin-pattern) 3)
                                 c
                               (format "[^%s]*%s" (substring pinyin-pattern 1 -1) pinyin-pattern)))))
                       ls "")
          (mapconcat (lambda (c)
                       (if (and (string= c "$")
                                (string-match "$\\'" pattern))
                           c
                         (format "[^%s]*%s" c (regexp-quote c))))
                     ls ""))
        ;; Fuzzy match.
        )))

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
                    (concat ".*" (regexp-quote bn))))))))

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
                                     (invalid-regexp nil))))))))

  ;;;;;;;;;;;;;;;;;;;;;method to add action;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (defun helm-my-test (candidate)
  ;;   (print candidate))
  ;; (setq helm-find-files-actions (append helm-find-files-actions '(("my-test" . helm-my-test))))

  :diminish (helm-mode)
  )

(provide 'init-helm)
;;; init-helm.el ends here
