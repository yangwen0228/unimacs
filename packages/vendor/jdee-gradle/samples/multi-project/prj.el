;;
;; JDEE gradle project file for the root of the multi-project example
;;

(require 'jdee-gradle)

(jdee-gradle-set-project "example" nil t)

(defconst example::subprojects
  (mapcar (lambda (d) (expand-file-name d jdee-gradle-project-root))
          '(
            ;; Insert subdirectory names here
            )
          "List of directories containing example sub-projects.")
  )

(defun example::subproject (name)
  "Define a example subproject.
This is intended to be called from the sub-project JDEE project (prj.el) file."
  (jdee-gradle-set-project-name name)
  (let ((dir (jdee-gradle-module-dir))
        (src-file (buffer-file-name)))
    ;; Load the generated prj file, if it exists
    (if (not (load (expand-file-name "prj-generated" jdee-gradle-project-root) t))
        ;; It wasn't found, so set some defaults
        (jdee-set-variables
         `(jdee-sourcepath ',(cl-mapcon (lambda (p) (list (expand-file-name "src/main/java" p)
                                                          (expand-file-name "src/test/java" p)))
                                        example::subprojects))
         `(jdee-build-class-path ',(cl-mapcon (lambda (p) (list (expand-file-name "build/classes/main" p)
                                                                (expand-file-name "build/classes/test" p)))
                                              example::subprojects))
         `(jdee-global-classpath ',(cl-mapcon (lambda (p) (list (expand-file-name "build/classes/main" p)
                                                                (expand-file-name "build/classes/test" p)))
                                              example::subprojects))
         ))
    ;; Other settings whose value depends on the particular sub-project go here
    (jdee-set-variables
     `(jdee-compile-option-directory ,(expand-file-name (if (and src-file (string-match-p "/test/" src-file))
                                                            "build/classes/test"
                                                          "build/classes/main") dir))
     `(jdee-run-working-directory ,dir)
     ))
  )

(jdee-set-variables
 `(jdee-jdk-doc-url ,(format "http://docs.oracle.com/javase/%s/docs/api/index.html" (jdee-java-minor-version)))
 `(jdee-help-docsets '(,@(cl-mapcar (lambda (p) (list p ,(concat "file://" (expand-file-name "build/docs/javadoc" p)) nil))
                                    example::subprojects)
                       (nil ,(format "http://docs.oracle.com/javase/%s/docs/api" (jdee-java-minor-version))
                            ,(format "1.%s" (jdee-java-minor-version)))
                       ))
 )

;;
;; Settings whose value is indendent of the particular subproject go here.
;; Note that these will apply to all sub-projects.
;;

;;
;; Add other settings here
;;

;;
;; Javadoc
;;
(jdee-set-variables 
 `(jdee-help-docsets '(,@jdee-help-docsets
                       ("TestNG" "http://testng.org/javadocs" nil)
                       ("JUnit" "http://junit.org/junit4/javadoc/latest" nil)))
 )

;;
;; Coding style
;;
(jdee-set-variables 
 ;; Organize imports
 `(jdee-import-auto-sort t)
 `(jdee-import-auto-sort-function 'jdee-import-organize)
 `(jdee-import-group-of-rules '(("^com\\.mycorp" . "00_mycorp")
                                ("^mycorp" . "10_mycorp")
                                ("^com" . "20_com")
                                ("^org" . "30_org")
                                ("^java" . "99_java")
                                (".+" . "90_other")
                                ))
 `(jdee-import-default-group-name "90_other")
 `(jdee-import-sorted-groups 'gor)
 ;; Wizards and generated code
 `(jdee-gen-final-methods nil)
 `(jdee-wiz-tostring-prefix "{")
 `(jdee-wiz-tostring-postfix "}")
 `(jdee-wiz-tostring-static-members nil)
 `(jdee-javadoc-version-tag-template nil)
 `(jdee-javadoc-author-tag-template nil)
 )

;;
;; Other tools
;;
(jdee-set-variables
 `(jdee-checkstyle-style (expand-file-name "src/main/config/checkstyle/checkstyle.xml" jdee-gradle-project-root))
 `(jdee-xref-store-prefixes '("com.mycorp"))
 )
