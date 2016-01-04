;;; init-funcs.el --- Define the common functions used in other configure files.
;;; Commentary:
;; This package must be require at the first place, when the program starts.
;; Some functions defined in the file are required by other packages.

;;; Code:

(defun add-to-hooks (fun hooks)
  "Add function to hooks"
  (dolist (hook hooks)
    (add-hook hook fun)))
(defun add-all-to-hook (hook &rest funs)
  "Add functions to hook."
  (add-to-hook hook funs))
(defun add-to-hook (hook funs)
  "Add list of functions to hook."
  (dolist (fun funs)
    (add-hook hook fun)))

(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))


;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

;;----------------------------------------------------------------------------
;; Find the directory containing a given library
;;----------------------------------------------------------------------------
(autoload 'find-library-name "find-func")
(defun directory-of-library (library-name)
  "Return the directory in which the `LIBRARY-NAME' load file is found."
  (file-name-as-directory (file-name-directory (find-library-name library-name))))

;;----------------------------------------------------------------------------
;; Browse current HTML file
;;----------------------------------------------------------------------------
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (browse-url-generic (concat "file://" (buffer-file-name))))

(require 'cl)

(defmacro with-selected-frame (frame &rest forms)
  (let ((prev-frame (gensym))
        (new-frame (gensym)))
    `(progn
       (let* ((,new-frame (or ,frame (selected-frame)))
              (,prev-frame (selected-frame)))
         (select-frame ,new-frame)
         (unwind-protect
             (progn ,@forms)
           (select-frame ,prev-frame))))))

(provide 'init-funcs)
;;; init-funcs.el ends here