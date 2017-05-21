;;
;; JDEE gradle project file for example
;;

(require 'jdee-gradle)

(jdee-gradle-set-project "example" nil t)

;; Load the generated prj file, if it exists
(if (not (load (expand-file-name "prj-generated" jdee-gradle-project-root) t))
    ;; It wasn't found, so set some defaults
    (jdee-set-variables
     `(jdee-sourcepath '("./src/main/java" "./src/test/java"))
     `(jdee-build-class-path '("./build/classes/main" "./build/classes/test"))
     `(jdee-global-classpath '("./build/classes/main" "./build/classes/test"))
     ))

(jdee-set-variables
 `(jdee-jdk-doc-url ,(format "http://docs.oracle.com/javase/%s/docs/api/index.html" (jdee-java-minor-version)))
 `(jdee-compile-option-directory ,(let ((src-file (buffer-file-name)))
                                    (if (and src-file (string-match-p "/test/" src-file))
                                        "./build/classes/test"
                                      "./build/classes/main")))
 `(jdee-help-docsets '(("example" ,(concat "file://" (expand-file-name "build/docs/javadoc" jdee-gradle-project-root)) nil)
                       (nil ,(format "http://docs.oracle.com/javase/%s/docs/api" (jdee-java-minor-version))
                            ,(format "1.%s" (jdee-java-minor-version)))
                       ))
 `(jdee-run-working-directory ,jdee-gradle-project-root)
 )

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
 `(jdee-xref-db-base-directory ,(expand-file-name "build/jdee" jdee-gradle-project-root))
 `(jdee-xref-store-prefixes '("com.mycorp"))
 )
