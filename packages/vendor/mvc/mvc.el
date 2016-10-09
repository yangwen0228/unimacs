;;; mvc.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(require 'projectile)
(require 'f)
(defconst mvc-version "1.0")

(defgroup mvc nil
  "Create mvc files for tcl-mode."
  :group 'convenience)

(defcustom mvc-test t
  "Create test files."
  :type 'boolean
  :group 'mvc)

(defun mvc-create-module-oo (dir name)
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
      (f-write-text
       (concat "if {[info commands ::modules::" name "::Model] ne \"\"} {\n"
               "    return\n"
               "}\n"
               "\n"
               "package require TclOO\n"
               "oo::class create ::modules::" name "::Model {\n"
               "    constructor {} {\n"
               "    }\n"
               "}\n"
               "# Public APIs: procname's first letter should be lower case\n\n"
               "# Private APIs: procname's first letter should be upper case")
       'utf-8 model)
      (f-write-text
       (concat "if {[info commands ::modules::" name "::View] ne \"\"} {\n"
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
               "oo::class create ::modules::" name "::View {\n"
               "    constructor {} {\n"
               "        ::msgcat::mcload [file join $modules::" name "::V_Dir locales]\n"
               "        ::msgcat::mcload $modules::" name "::V_LocalesDir\n"
               "    }\n"
               "}\n"
               "# Public APIs: procname's first letter should be lower case\n"
               "::oo::define ::modules::" name "::View method buildGUI {frm} {\n"
               "}\n"
               "# Private APIs: procname's first letter should be upper case")
       'utf-8 view)
      (f-write-text
       (let ((classname (upcase-initials name)))
         (concat "if {[info commands ::modules::" classname "] ne \"\"} {\n"
                 "    return\n"
                 "}\n"
                 "\n"
                 "package require TclOO\n"
                 "oo::class create ::modules::" classname " {\n"
                 "    variable View Model\n"
                 "    constructor {} {\n"
                 "        set View  [::modules::" name "::View  new]\n"
                 "        set Model [::modules::" name "::Model new]\n"
                 "    }\n"
                 "    destructor {\n"
                 "        ::cnhc::oo::destroyObjsByVarName View Model\n"
                 "    }\n"
                 "}\n"
                 "# Public APIs: procname's first letter should be lower case\n"
                 "::oo::define ::modules::" classname " method buildGUI {frm} {\n"
                 "    $View buildGUI $frm\n"
                 "}\n"
                 "# Private APIs: procname's first letter should be upper case"))
       'utf-8 control)
      (f-write-text
       (concat "::msgcat::mcset cn \"\" \"\"")
       'utf-8 locale)
      (find-file view))))

(defun mvc-create-module-proc (dir name)
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
      (f-write-text
       (concat "if {[namespace exists ::modules::" name "::model]} {\n"
               "    return\n"
               "}\n"
               "\n"
               "namespace eval ::modules::" name "::model {\n"
               "    variable V_Dir [file normalize [file dir [info script]]]\n"
               "}\n"
               "# Public APIs: procname's first letter should be lower case\n\n"
               "# Private APIs: procname's first letter should be upper case")
       'utf-8 model)
      (f-write-text
       (concat "if {[namespace exists ::modules::" name "::view]} {\n"
               "    return\n"
               "}\n"
               "namespace eval modules::" name "::view {\n"
               "    variable V_Dir [file dir [info script]]\n"
               "    ::msgcat::mcload [file join $V_Dir locales]\n"
               "    proc loadLocale {dir} {\n"
               "        ::msgcat::mcload $dir\n"
               "    }\n"
               "}\n"
               "\n"
               "# Public APIs: procname's first letter should be lower case\n"
               "proc ::modules::" name "::view::buildGUI {frm control} {\n"
               "    set frm [yattk::labelframe [::hwt::GetUniqueFrame $frm] -text [mc " name "] -padding {10 0 0 0}]\n"
               "    pack $frm -side top -fill x -pady {0 0}\n"
               "}\n"
               "# Private APIs: procname's first letter should be upper case")
       'utf-8 view)
      (f-write-text
       (let ((classname (upcase-initials name)))
         (concat "if {[info commands ::modules::" classname "] ne \"\"} {\n"
                 "    return\n"
                 "}\n"
                 "\n"
                 "package require TclOO\n"
                 "oo::class create ::modules::" classname " {\n"
                 "    constructor {} {\n"
                 "    }\n"
                 "    destructor {\n"
                 "    }\n"
                 "}\n"
                 "# Public APIs: procname's first letter should be lower case\n"
                 "::oo::define ::modules::" classname " method buildGUI {frm} {\n"
                 "    ::modules::" name "::view::buildGUI $frm [self]\n"
                 "}\n"
                 "::cnhc::hm::templateOOMethods {Nastran} {\n"
                 "    ::oo::define ::modules::" classname " method onSample {args} {\n"
                 "        ::cnhc::hm::performance on\n"
                 "        ::cnhc::hm::performance off\n"
                 "    }\n"
                 "}\n"
                 "# Private APIs: procname's first letter should be upper case\n"
                 ""))
       'utf-8 control)
      (f-write-text
       (concat "::msgcat::mcset cn \"\" \"\"")
       'utf-8 locale)
      (find-file view))))

(provide 'mvc)
;;; mvc.el ends here
