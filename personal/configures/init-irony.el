;;; init-irony.el --- configure for irony mode
;;; Commentary:
;; comments

;;; Code:

;; Configure for irony-mode:
(if *win32* (setq w32-pipe-read-delay 0))
(add-hook 'c++-mode-hook  'irony-mode)
(add-hook 'c-mode-hook    'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(defun my-irony-mode-hook ()
  ;; FIXBUG: when the file contains chinese:
  (defun irony--buffer-size-in-bytes ()
    "Return the buffer size, in bytes."
    (let* ((char-max (point-max))
           (byte-max (position-bytes char-max)))
      (if (= char-max byte-max)
          ;; No chinese:
          (1- char-max)
        ;; Have chinese:
        (- byte-max (/ (- byte-max char-max) 2) 1))))
  (defun irony--server-process-sentinel (process event)
    (unless (process-live-p process)
      (setq irony--server-process nil)
      ;; No error message:
      ;; (message "irony process stopped!")
      ))
  (defun irony--send-parse-request (request callback &rest args)
    (let ((process (irony--get-server-process-create))
          (argv (append (list request
                              "--num-unsaved=1"
                              (irony--get-buffer-path-for-server))
                        args))
          (compile-options (irony--adjust-compile-options)))
      (when (and process (process-live-p process))
        (irony--server-process-push-callback process callback)
        ;; (print (combine-and-quote-strings argv))
        ;; (print (irony--buffer-size-in-bytes))
        (irony--without-narrowing
          (process-send-string process
                               (format "%s\n%s\n%s\n%d\n"
                                       (combine-and-quote-strings argv)
                                       (combine-and-quote-strings compile-options)
                                       buffer-file-name
                                       (irony--buffer-size-in-bytes)))
          (process-send-region process (point-min) (point-max))
          ;; always make sure to finish with a newline (required by irony-server
          ;; to play nice with line buffering even when the file doesn't end with
          ;; a newline)
          (process-send-string process "\n")
          ))))



  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async)
  (cond
   (*win32*
    (setq irony--server-executable (expand-file-name "extra-bins/irony/bin/irony-server.exe" unimacs-utils-dir))
    ;; (setenv "LD_LIBRARY_PATH" (expand-file-name "extra-bins/irony/lib" unimacs-utils-dir))
    )
   (*is-a-mac*
    (setq irony--server-executable (expand-file-name "extra-bins/irony/bin/irony-server" unimacs-utils-dir))
    (setenv "LD_LIBRARY_PATH" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib"))
   ))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(defun my-create-irony-project (dir)
  (interactive
   (list (read-directory-name "Root Directory: ")))
  (let* ((default-directory dir)
        (file (expand-file-name ".clang_complete" default-directory)))
    (with-temp-file file
      (insert "-IC:/Program Files (x86)/Microsoft Visual Studio 12.0/VC/include\n")
      (insert "-include ../config.h\n")
      (insert "-I../include\n"))
    (message "The .clang_complete file has been created successfully!")
    ))

(provide 'init-irony)
;;; init-irony.el ends here
