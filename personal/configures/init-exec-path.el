(setq my-extra-bin-dir (expand-file-name "extra-bins" unimacs-utils-dir))


(eval-after-load 'exec-path-from-shell
  '(progn
     (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"))
       (add-to-list 'exec-path-from-shell-variables var))))


(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(cond
 (*win32*
  (progn
    (dolist (path '("Aspell/bin"
                    "glo651wb/bin"
                    "ctags5.8/bin"
                    "irony/bin"
                    "msys64/bin"
                    ))
      (let ((full-path (expand-file-name path my-extra-bin-dir)))
        (setenv "PATH" (concat (replace-regexp-in-string "\/" "\\\\" full-path) ";" (getenv "PATH")))
        (add-to-list 'exec-path full-path)))
    (setenv "GTAGSCONF" (expand-file-name "glo651wb/share/gtags/gtags.conf" my-extra-bin-dir))
    ))
 (*is-a-mac*
  (progn
    (setenv "PATH" (concat "/usr/local/texlive/2014/bin/universal-darwin:" (getenv "PATH")))
    (add-to-list 'exec-path "/usr/local/texlive/2014/bin/universal-darwin")
    )
 ))

(provide 'init-exec-path)
