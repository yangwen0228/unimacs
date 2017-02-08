;;; init-helm-projectile.el --- projectile work with helm
;;; Commentary:
;; comments

;;; Code:
(use-package helm-projectile
  :init
  (helm-projectile-on)
  (use-package projectile
    :preface
    (defun projectile-create-a-project (dir)
      (interactive "D")
      (let ((file (expand-file-name ".projectile" dir)))
        (with-temp-file file
          (erase-buffer)
          (insert "
;; - means ignore this pattern, must has a lead \"/\", using the regexp rule.
;; + means add this subdir, it will block the root dir.
-/GTAGS
-/GPATH
-/GRTAGS
-/\\.svn
-/\\.git
-/icons
-/images
-/libs/encoding
-/**.dll
")
          )))

    :init
    (projectile-mode t)
    (setq projectile-project-root-files-functions
          '(projectile-root-bottom-up
            projectile-root-top-down
            ;; projectile-root-top-down-recurring ; don't use svn to define root.
            ))
    ;; Bugfix: support Chinese file path.
    (defun projectile-files-via-ext-command (command)
      "Get a list of relative file names in the project root by executing COMMAND."
      (condition-case nil
          (let* ((items (split-string command " "))
                 (cmd (nth 0 items))
                 (opts (subseq items 1)))
            (with-temp-buffer
              (apply #'call-process cmd nil t nil opts)
              (split-string (buffer-string) "\0" t)))
        (error nil)))

    (defun projectile-get-ext-command ()
      "Determine which external command to invoke based on the project's VCS."
      (let ((vcs (projectile-project-vcs)))
        (cond
         ;; ((eq vcs 'git) projectile-git-command)
         ((eq vcs 'hg) projectile-hg-command)
         ((eq vcs 'fossil) projectile-fossil-command)
         ((eq vcs 'bzr) projectile-bzr-command)
         ((eq vcs 'darcs) projectile-darcs-command)
         ;; ((eq vcs 'svn) projectile-svn-command) ; don't use svn to search files.
         (t projectile-generic-command))))

    (defun projectile-remove-ignored (files &optional subdirectories)
      "Remove ignored files and folders from FILES.

Operates on filenames relative to the project root.  Optionally,
you can filter ignored files in subdirectories by setting
SUBDIRECTORIES to a non-nil value."
      (let ((ignored (append (projectile-ignored-files-rel)
                             (projectile-ignored-directories-rel))))
        (-remove (lambda (file)
                   ;; string-prefix-p -> string-match-p
                   (or (--any-p (string-match-p it (if subdirectories
                                                       (file-name-nondirectory file)
                                                     file))
                                ignored)
                       (--any-p (string-suffix-p it file) projectile-globally-ignored-file-suffixes)))
                 files)))

    (defun projectile-parse-dirconfig-file ()
      "Parse project ignore file and return directories to ignore and keep.

The return value will be a list of three elements, the car being
the list of directories to keep, the cadr being the list of files
or directories to ignore, and the caddr being the list of files
or directories to ensure.

Strings starting with + will be added to the list of directories
to keep, and strings starting with - will be added to the list of
directories to ignore.  For backward compatibility, without a
prefix the string will be assumed to be an ignore string."
      (let (keep ignore ensure (dirconfig (projectile-dirconfig-file)))
        (when (projectile-file-exists-p dirconfig)
          (with-temp-buffer
            (insert-file-contents dirconfig)
            (while (not (eobp))
              (pcase (char-after)
                (?+ (push (buffer-substring (1+ (point)) (line-end-position)) keep))
                (?- (push (buffer-substring (1+ (point)) (line-end-position)) ignore))
                (?! (push (buffer-substring (1+ (point)) (line-end-position)) ensure))
                ;; add comment support, not - + begin, as comment
                ;; (_  (push (buffer-substring     (point)  (line-end-position)) ignore))
                )
              (forward-line)))
          (list (--map (file-name-as-directory (projectile-trim-string it))
                       (delete "" (reverse keep)))
                (-map  #'projectile-trim-string
                       (delete "" (reverse ignore)))
                (-map  #'projectile-trim-string
                       (delete "" (reverse ensure)))))))

    (setq projectile-svn-command "svn list -R . | grep -v '$/")
    (setq projectile-git-submodule-command
          "git submodule --quiet foreach 'echo $path'")

    (setq projectile-completion-system 'helm)
    ;; The alien is faster, but not well support for Windows.
    ;; The native always works, but slower.
    ;; To force the use of external indexing in Windows:
    (setq projectile-indexing-method 'alien)
    ;;(setq projectile-indexing-method 'native)
    ;; (setq projectile-indexing-method nil)

    ;; Use cache for big project.
    (setq projectile-enable-caching t)

    (defun my-projectile-project-name ()
      "Return project name."
      (if projectile-project-name
          projectile-project-name
        (let ((project-root (condition-case nil
                                (projectile-project-root)
                              (error nil))))
          (if project-root
              (let ((project-name (funcall projectile-project-name-function project-root)))
                (if (string= project-name "source codes")
                    (funcall projectile-project-name-function (file-name-directory (directory-file-name project-root)))
                  project-name))
            "-"))))

    (setq projectile-mode-line
          '(:eval (format " Pj[%s]" (my-projectile-project-name))))))

(provide 'init-helm-projectile)
;;; init-helm-projectile.el ends here