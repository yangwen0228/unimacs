;;; mvc.el --- Summary
;;; Commentary:
;; comments

;;; Code:
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
  ;; (unless (file-exists-p dir)
  ;;     (make-directory dir t))
  (when (or (not (file-exists-p (expand-file-name name dir)))
            (and (file-exists-p (expand-file-name name dir))
                 (yes-or-no-p (format "The module %s is already exist, do you want to overwrite?" name))))
    (if (file-exists-p (expand-file-name name dir))
        (delete-directory (expand-file-name name dir))
      (make-directory (expand-file-name name dir) t))
    (let* ((module-dir (expand-file-name name dir))
           (model       (expand-file-name "model.tcl" module-dir))
           (view        (expand-file-name "view.tcl"  module-dir))
           (control-dir (expand-file-name "control" module-dir))
           (control     (expand-file-name "control/control.tcl" module-dir))
           (locale-dir  (expand-file-name "locales" module-dir))
           (locale      (expand-file-name "locales/cn.msg" module-dir)))
      (make-directory control-dir t)
      (make-directory locale-dir t)
      (with-temp-file model
        (insert "catch {namespace delete modules::" name "::model}\n"
                "\n"
                "namespace eval modules::" name "::model {\n"
                "}\n"
                "\n"
                "# Public APIs: procname's first letter should be lower case\n\n"
                "# Private APIs: procname's first letter should be upper case"))
      (with-temp-file view
        (insert "if {[info commands ::modules::" name "::View] ne \"\"} {\n"
                "    return\n"
                "}\n"
                "namespace eval modules::" name " {\n"
                "    variable V_Dir [file dir [info script]]\n"
                "    variable V_LocalesDir [list]\n"
                "    proc setLocalesDir {dir} {\n"
                "        variable V_LocalesDir $dir\n"
                "    }\n"
                "}\n"
                "\n"
                "package require TclOO\n"
                "oo::class create modules::" name "::View {\n"
                "    constructor {} {\n"
                "        mcload [file join $modules::" name "::V_Dir locales]\n"
                "        mcload $modules::" name "::V_LocalesDir\n"
                "    }\n"
                "}\n"
                "# Public APIs: procname's first letter should be lower case\n\n"
                "# Private APIs: procname's first letter should be upper case"))
      (with-temp-file control
        (let ((classname (upcase-initials name)))
          (insert "if {[info commands ::modules::" classname "] ne \"\"} {\n"
                  "    return\n"
                  "}\n"
                  "\n"
                  "package require TclOO\n"
                  "oo::class create modules::" classname " {\n"
                  "    superclass modules::" name "::View\n"
                  "    constructor {frm xmlFile} {\n"
                  "    }\n"
                  "}\n"
                  "# Public APIs: procname's first letter should be lower case\n\n"
                  "# Private APIs: procname's first letter should be upper case")))
      (with-temp-file locale
        (insert "::msgcat::mcset cn \"\" \"\""))
      (find-file view)
      )))

(provide 'mvc)
;;; mvc.el ends here
