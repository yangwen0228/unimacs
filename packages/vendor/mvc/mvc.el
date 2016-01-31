(require 'projectile)
(defconst mvc-version "1.0")

(defgroup mvc nil
  "Create mvc files for tcl-mode."
  :group 'convenience)

(defcustom mvc-test t
  "Create test files."
  :type 'boolean
  :group 'mvc)

(defun mvc-create-module (dir name)
  (interactive
   (list
    (file-name-as-directory
     (read-directory-name "The modules directory: "
                          (condition-case nil
                              (expand-file-name "modules" (projectile-project-root))
                            (error nil)) nil nil))
    (read-string "The module name (module): " nil nil "module")))
  (when (or (not (file-exists-p (expand-file-name name dir)))
            (and (file-exists-p (expand-file-name name dir))
                 (yes-or-no-p (format "The module %s is already exist, do you want to overwrite?" name))))
    (if (file-exists-p (expand-file-name name dir))
        (delete-directory (expand-file-name name dir))
      (make-directory (expand-file-name name dir)))
    (let ((model       (expand-file-name "model.tcl" dir))
          (view        (expand-file-name "view.tcl"  dir))
          (control-dir (expand-file-name "control" dir))
          (control     (expand-file-name "control/control.tcl" dir))
          (locale-dir  (expand-file-name "locales" dir))
          (locale      (expand-file-name "cn.msg" dir)))
      (make-directory control-dir)
      (make-directory locale-dir)
      (with-temp-file model
        (insert "catch {namespace delete modules::" name "::model}\n"
                "\n"
                "namespace eval ::modules::" name "model {\n"
                "}\n"
                "\n"
                "# Public APIs: procname's first letter should be lower case\n"
                "# Private APIs: procname's first letter should be upper case"))
      )))

(provide 'mvc)