;; Use w3m to view the html documentation.
(defgroup html-help nil
  "Use w3m to view the html documentation. Use helm to complete the keywords."
  :group 'help)

(defcustom html-help-lookup-alist
  '(("tcl-mode" . ("")))
  "Use prog mode and html dirs to define the help documentation."
  :type '(repeat (string :tag "Program mode")
                 (list :tag "Html dirs"))
  :group 'html-help)

(defun html-help-parse-dir-files (dir &optional alist)
  "Recursively add all pairs (FILE . PATH) under DIR to ALIST."
  (dolist (file (directory-files dir t) alist)
    (let ((filename (file-name-nondirectory file)))
      (cond
       ((not (file-directory-p file))
        (if (not (string= (subseq filename -5 -5) ")"))
          (push (cons filename file) alist)))
       ((member filename '("." "..")))
       (t (setq alist (tcl-hm-files-alist file alist)))))))

(provide 'html-help)