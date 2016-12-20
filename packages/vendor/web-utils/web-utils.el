(defun web-utils-get-all-images (dir)
  "Get all image paths under DIR"
  (interactive "DSelect directory:")
  (let ((dir-name (file-name-nondirectory (directory-file-name dir))))
    (dolist (file (directory-files dir))
      (if (string-match "\.\\(png\\)\\|\\(jpg\\)" file)
          (princ (concat "\"" dir-name "\/" file "\",\n"))))))
