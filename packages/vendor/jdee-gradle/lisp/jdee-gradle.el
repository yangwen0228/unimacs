;;; jdee-gradle.el --- Gradle support for JDEE       -*- lexical-binding: t; -*-

;; Copyright (C) 2016

;; Author: Stan Lanning <lanning at pobox dot com>
;; Keywords: java, tools, gradle

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file adds support to JDEE for projects that use Gradle.

;; Requirements:
;;   * The gradle-el package; see https://github.com/vhallac/gradle-el
;;   * A recent version of JDEE

;; Optional:
;;   * The gradle "jdee" task definition; see http://ignatyev-dev.blogspot.com/2013/07/gradle-projects-in-jdee.html.
;;     Edit it so it produces the file "prj-generated.el" instead of "prj.el".

;; Usage:
;; Put this file somewhere in your elisp search-path.
;; You might want to byte-compile it, but that isn't necessary.
;; The "entry point" to this code is the function `jdee-gradle-set-project':
;; that note properties of the Gradle build, and binds the JDEE build function to use Gradle.
;;
;; For a simple gradle project (without sub-projects), visit the prj.el file at the root of the project.
;; The function M-x jdee-gradle-gen-single-project-file to insert a simple
;; JDEE project definition into the current buffer.

;; For a gradle multi-project (with sub-projects), first visit the prj.el file at the root of the project
;; and call the interactive command M-x jdee-gradle-gen-multi-project-file.
;; Then for each sub-project visit the prj.el file at the root of the sub-project
;; and call the interactive command M-x jdee-gradle-gen-sub-project-file.

;; If you are using the gradle jdee task, edit the build.gradle file and add the code
;;
;;    apply from: "jdee-gradle-install-directory/jdee.gradle"
;;
;; or, for the root of a multi-project,
;;
;;    allprojects {
;;        apply from: "${rootDir}/src/ide/emacs/jdee.gradle"
;;    }
;;
;; Then run "gradlew assemble jdee".

;;; Code:

(require 'gradle)
(require 'cl-lib)
(require 'jdee)

(defgroup jdee-gradle nil
  "JDEE Gradle"
  :group 'jdee
  :prefix "jdee-gradle-")

(defvar jdee-gradle-project-root nil
  "*Base directory for the Gradle project.
For sub-projects, this is the directory of the top-level project.")
(make-variable-buffer-local 'jdee-gradle-project-root)

(defvar jdee-gradle-project-name nil
  "Name of the Gradle project.
For sub-projects, this is the name of the top-level project.")
(make-variable-buffer-local 'jdee-gradle-project-name)

(defvar jdee-gradle-subproject-root nil
  "Root directory of the Gradle subproject, if any.")
(make-variable-buffer-local 'jdee-gradle-subproject-root)

(defvar jdee-gradle-subproject-name nil
  "Name of the Gradle sub-project, if any.")
(make-variable-buffer-local 'jdee-gradle-subproject-name)

(defun jdee-gradle-module-dir ()
  "The name of this gradle (sub)project root directory."
  (or jdee-gradle-subproject-root jdee-gradle-project-root))

;;;###autoload
(defun jdee-gradle-set-project (&optional name dir force-top-level-p)
  "Define a Gradle project.
Automatically determines if the project is a sub-project of a containing multi-project
unless force-top-level-p is true.
If not given, dir defaults to the directory containing the project file currently being loaded.
For top-level projects, name defaults to the base name of dir; for sub-projects it defaults to
MAIN-BASE where MAIN is the name of the top-level project and BASE is the base name of dir.
Returns the value of dir that was found."
  (cond ((not (null dir)) )
        ((not (null jdee-loading-project-file))
         (setq dir (file-name-directory jdee-loading-project-file)))
        (t (error "No directory given")))
  (if (not (file-exists-p (expand-file-name "build.gradle" dir)))
         (error "No Gradle build file found in %s" dir))
  (if (null name)
      (setq name (let ((base (file-name-base dir)))
                   (if jdee-gradle-project-name
                       (concat jdee-gradle-project-name "-" base)
                     base))))
  (if (and jdee-gradle-project-root (not force-top-level-p))
      ;; Already found a gradle project higher up in the food chain; assume this is a sub-project
      (setq jdee-gradle-subproject-root dir
            jdee-gradle-subproject-name name)
    ;; Top-level project: either a single project or the root of a multi-project
    (setq jdee-gradle-project-root dir
          jdee-gradle-project-name name
          jdee-gradle-subproject-root nil
          jdee-gradle-subproject-name nil))
  (jdee-set-project-name name)
  (jdee-set-variables
   `(jdee-build-function 'jdee-gradle-build)
   )
  dir)

(defun jdee-gradle-get-default-directory () 
  "Gets the default-directory according to the value of
`jdee-gradle-project-root'."
  (if (or (null jdee-gradle-project-root) (string= jdee-gradle-project-root ""))
      default-directory
    jdee-gradle-project-root))

(defun jdee-gradle-with-project-root (fn)
  "Execute fn in the Gradle root project directory.
See `gradle-with-project-root-func'."
  (let ((default-directory (jdee-gradle-get-default-directory)))
    (gradle--with-current-directory fn)))

;;
;; Hooks when visiting a file in a Gradle JDEE project
;;

;;;###autoload
(defcustom jdee-gradle-project-hooks nil
  "Specifies a list of functions to be run when a gradle JDEE project
becomes active."
  :group 'jdee-gradle
  :type '(repeat (function :tag "Function")))

(defun jdee-gradle-project-hook ()
  "JDEE project hook that runs hooks on `jdee-gradle-project-hooks'
if the current project is a gradle project."
  (if jdee-gradle-project-root
      (run-hooks 'jdee-gradle-project-hooks)))

(add-hook 'jdee-project-hooks 'jdee-gradle-project-hook)

;;
;; Building with Gradle
;;

(defcustom jdee-gradle-build-options '("--console=plain")
  "List of additional options to pass to gradle."
  :group 'jdee-gradle
  :type 'list)

(defun jdee-gradle-compile-fixup-task-newlines (buffer _msg)
  ;; Gradle can leave the task name (like ":myproj:compileJava") at the beginning of a line, resulting in lines like
  ;; :myproj:compileJava/home/myname/myproj/src/main/java/com/mycorp/myproj/MyClass.java:84: error: invalid method declaration; return type required
  ;; This function scans over the buffer, looking for likely such lines, and inserts an appropriate newline.
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (re-search-forward "^\\(:[^/]+\\)\\(/.+:[0-9]+:\\)" nil t)
        (replace-match "\\1\n\\2" nil nil)))))

(defmacro jdee-with-compile-finish-fn (cleanup &rest body)
  "Install a post-compile cleanup fn."
  (declare (indent 1))
  ;; This is complicated beause the finish functions are called asynchronously,
  ;; and so simple dynamic binding doesn't work.
  `(progn
     (setq compilation-finish-functions 
           (lambda (buf msg)
             (funcall ,cleanup buf msg)
             (run-hook-with-args 'jdee-compile-finish-hook buf msg)
             (setq compilation-finish-functions nil)))
     ,@body))

(defcustom jdee-gradle-get-tasks-function 'jdee-gradle-get-tasks
  "Function to call to get the list of tasks to execute when doing a Gradle build.
See also `jdee-gradle-build-options'."
  :group 'jdee-gradle
  :type 'function)

(defun jdee-gradle-get-tasks ()
  "Gets the Gradle task(s) to run when doing a build;
see `jdee-gradle-get-tasks-function'.
This runs Gradle to get a list of all available tasks,
and then prompts you pick from that list."
  (gradle--input-commandline))

;; Optional code to prompt only with subproject tasks

(defun jdee-gradle-subproject-filter-tasks (tasks)
  "Filters a list of Gradle tasks to include only those matching the current subproject name."
  (if jdee-gradle-subproject-name
      (cl-remove-if (lambda (x) 
                      (not (string-match (concat "\\(^-\\|" (regexp-quote jdee-gradle-subproject-name) ":\\)") x)))
                    tasks)
    tasks))

(defvar jdee-gradle-get-task-filter nil
  "Filter function to apply to list of tasks found by `gradle--get-task-list'.")

(defadvice gradle--get-task-list (after jdee-gradle-get-task-filter activate)
  "Apply the value of `jdee-gradle-get-task-filter' to the tasks that are found."
  (if jdee-gradle-get-task-filter
      (setq ad-return-value (funcall jdee-gradle-get-task-filter ad-return-value))))

(defun jdee-gradle-get-subproject-tasks ()
  "Gets the Gradle task(s) to run when doing a build;
see `jdee-gradle-get-tasks-function'.
This runs Gradle to get a list of all available tasks
and filters out those not appropriate for the current subproject."
  (let ((jdee-gradle-get-task-filter 'jdee-gradle-subproject-filter-tasks))
    (gradle--input-commandline)))

;;;###autoload
(defun jdee-gradle-build ()
  "Invokes `gradle-run' to build the current project.
The tasks to execute are found by calling `jdee-gradle-get-tasks'.
This function can be used as the value of `jdee-build-function'."
  (interactive)
  (let* ((default-directory (jdee-gradle-get-default-directory))
         (gradle-with-project-root-func 'jdee-gradle-with-project-root))
    (jdee-with-compile-finish-fn 'jdee-gradle-compile-fixup-task-newlines
      (gradle-run (append jdee-gradle-build-options (funcall jdee-gradle-get-tasks-function))))))

;;
;; Generating project files
;; These assume a Maven structured code base for the project
;;

(defcustom jdee-gradle-gen-single-project-file-template
  '((p "Project name: " project-name t)
    ";;" > n
    ";; JDEE gradle project file for " (s project-name) > n
    ";;" > n
    n
    "(require 'jdee-gradle)" > n
    n
    "(jdee-gradle-set-project \"" (s project-name) "\" nil t)" > n
    n
    ";; Load the generated prj file, if it exists" > n
    "(if (not (load (expand-file-name \"prj-generated\" jdee-gradle-project-root) t))" > n
    "  ;; It wasn't found, so set some defaults" > n
    "  (jdee-set-variables" > n
    "   `(jdee-sourcepath '(\"./src/main/java\" \"./src/test/java\"))" > n
    "   `(jdee-build-class-path '(\"./build/classes/main\" \"./build/classes/test\"))" > n
    "   `(jdee-global-classpath '(\"./build/classes/main\" \"./build/classes/test\"))" > n
    "   ))" > n
    n
    "(jdee-set-variables" > n
    " `(jdee-jdk-doc-url ,(format \"http://docs.oracle.com/javase/%s/docs/api/index.html\" (jdee-java-minor-version)))" > n
    " `(jdee-compile-option-directory ,(let ((src-file (buffer-file-name)))" > n
    " (if (and src-file (string-match-p \"/test/\" src-file))" > n
    "     \"./build/classes/test\"" > n
    "   \"./build/classes/main\")))" > n
    " `(jdee-help-docsets '((\"" (s project-name) "\" ,(concat \"file://\" (expand-file-name \"build/docs/javadoc\" jdee-gradle-project-root)) nil)" > n
    "                       (nil ,(format \"http://docs.oracle.com/javase/%s/docs/api\" (jdee-java-minor-version))" > n
    "                            ,(format \"1.%s\" (jdee-java-minor-version)))" > n
    "                       ))" > n
    " `(jdee-run-working-directory ,jdee-gradle-project-root)" > n
    ")" > n
    n
    ";;" > n
    ";; Add other settings here" > n
    ";;" > n
    n
    )
  "Template for new jdee-gradle single-project file.
Setting this variable defines a template instantiation command
`jdee-gradle-gen-single-project-file' as a side-effect."
  :group 'jdee-gradle
  :type '(repeat string)
  :set (lambda (sym val)
         (tempo-define-template "jdee-gradle-single-project-file-template"
                                val
                                nil
                                "Insert a generic jdee-gradle single-project file.")
         (defalias 'jdee-gradle-gen-single-project-file
           `(lambda (is-interactive)
              (interactive "p")
              (let ((tempo-interactive is-interactive))
                (tempo-template-jdee-gradle-single-project-file-template))))
         (set-default sym val)))

(defcustom jdee-gradle-gen-multi-project-file-template
  '((p "Project name: " project-name t)
    ";;" > n
    ";; JDEE gradle project file for the root of the multi-project " (s project-name) > n
    ";;" > n
    n
    "(require 'jdee-gradle)" > n
    n
    "(jdee-gradle-set-project \"" (s project-name) "\" nil t)" > n
    n
    "(defconst " (s project-name) "::subprojects" > n
    "  (mapcar (lambda (d) (expand-file-name d jdee-gradle-project-root))" > n
    "          '(" > n
    "            ;; Insert subdirectory names here" > n
    "            )" > n
    "          \"List of directories containing " (s project-name) " sub-projects.\")" > n
    "  )" > n
    n
    "(defun " (s project-name) "::subproject (name)" > n
    "  \"Define a " (s project-name) " subproject." > n
    "This is intended to be called from the sub-project JDEE project (prj.el) file.\"" > n
    "  (jdee-gradle-set-project-name name)" > n
    "  (let ((dir (jdee-gradle-module-dir))" > n
    "        (src-file (buffer-file-name)))" > n
    "    ;; Load the generated prj file, if it exists" > n
    "    (if (not (load (expand-file-name \"prj-generated\" jdee-gradle-project-root) t))" > n
    "        ;; It wasn't found, so set some defaults" > n
    "        (jdee-set-variables" > n
    "         `(jdee-sourcepath ',(cl-mapcon (lambda (p) (list (expand-file-name \"src/main/java\" p)" > n
    "                                                          (expand-file-name \"src/test/java\" p)))" > n
    "                                        " (s project-name) "::subprojects))" > n
    "         `(jdee-build-class-path ',(cl-mapcon (lambda (p) (list (expand-file-name \"build/classes/main\" p)" > n
    "                                                                (expand-file-name \"build/classes/test\" p)))" > n
    "                                              " (s project-name) "::subprojects))" > n
    "         `(jdee-global-classpath ',(cl-mapcon (lambda (p) (list (expand-file-name \"build/classes/main\" p)" > n
    "                                                                (expand-file-name \"build/classes/test\" p)))" > n
    "                                              " (s project-name) "::subprojects))" > n
    "         ))" > n
    "    ;; Other settings whose value depends on the particular sub-project go here" > n
    "    (jdee-set-variables" > n
    "     `(jdee-compile-option-directory ,(expand-file-name (if (and src-file (string-match-p \"/test/\" src-file))" > n
    "                                                            \"build/classes/test\"" > n
    "                                                          \"build/classes/main\") dir))" > n
    "     `(jdee-run-working-directory ,dir)" > n
    "     ))" > n
    "  )" > n
    n
    "(jdee-set-variables" > n
    " `(jdee-jdk-doc-url ,(format \"http://docs.oracle.com/javase/%s/docs/api/index.html\" (jdee-java-minor-version)))" n >
    " `(jdee-help-docsets '(,@(cl-mapcar (lambda (p) (list p ,(concat \"file://\" (expand-file-name \"build/docs/javadoc\" p)) nil))" n >
    "                                    " (s project-name) "::subprojects)" > n
    "                       (nil ,(format \"http://docs.oracle.com/javase/%s/docs/api\" (jdee-java-minor-version))" > n
    "                       ,(format \"1.%s\" (jdee-java-minor-version)))" > n
    "                   ))" > n
    " )" > n
    n
    ";;" > n
    ";; Settings whose value is indendent of the particular subproject go here." > n
    ";; Note that these will apply to all sub-projects." > n
    ";;" > n
    n
    ";;" > n
    ";; Add other settings here" > n
    ";;" > n
    n
    )
  "Template for new jdee-gradle project file.
Setting this variable defines a template instantiation command
`jdee-gradle-gen-multi-project-file' as a side-effect."
  :group 'jdee-gradle
  :type '(repeat string)
  :set (lambda (sym val)
         (tempo-define-template "jdee-gradle-multi-project-file-template"
                                val
                                nil
                                "Insert a generic jdee-gradle multi-project file.")
         (defalias 'jdee-gradle-gen-multi-project-file
           `(lambda (is-interactive)
              (interactive "p")
              (let ((tempo-interactive is-interactive))
                (tempo-template-jdee-gradle-multi-project-file-template))))
         (set-default sym val)))

(defcustom jdee-gradle-gen-sub-project-file-template
  '((p "Main project name: " project-name t)
    (p "Full sub-project name: " sub-project-name t)
    ";;" > n
    ";; JDEE gradle project file for the sub-project " (s sub-project-name) > n
    ";; Note that this gets loaded after the top-level project file," > n
    ";; so any setting here will override values set there." > n
    ";;" > n
    n
    "(" (s project-name) "::subproject \"" (s sub-project-name) "\")" > n
    n
    ";;" > n
    ";; Other sub-project specific settings go here." > n
    ";;" > n
    n
    )
  "Template for new jdee-gradle project file.
Setting this variable defines a template instantiation command
`jdee-gradle-gen-sub-project-file' as a side-effect."
  :group 'jdee-gradle
  :type '(repeat string)
  :set (lambda (sym val)
         (tempo-define-template "jdee-gradle-sub-project-file-template"
                                val
                                nil
                                "Insert a generic jdee-gradle sub-project file.")
         (defalias 'jdee-gradle-gen-sub-project-file
           `(lambda (is-interactive)
              (interactive "p")
              (let ((tempo-interactive is-interactive))
                (tempo-template-jdee-gradle-sub-project-file-template))))
         (set-default sym val)))

(provide 'jdee-gradle)

;;; jdee-gradle.el ends here
