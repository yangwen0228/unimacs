;;; ensime-generate.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(defgroup ensime-generate nil
  "Create .ensime file for ivy project by ivy.xml."
  :group 'convenience)

(defvar ensime-generate-template (expand-file-name
                                  "ensime-template.el"
                                  (file-name-directory load-file-name)))
(defvar ensime-generate-maven-repo "c:/Users/yangwen283/.m2/repository")
(defvar ensime-generate-maven-root "d:/portable/apache-maven-3.5.0")
(defvar ensime-generate-jdk8 "c:/Program Files/Java/jdk1.8.0_121")

(defun ensime-generate-for-ivy (ivy-dir)
  "Create .ensime file for ivy project by ivy.xml."
  (interactive
   (list
    (file-name-as-directory
     (read-directory-name "The modules directory: "
                          (condition-case nil
                              (projectile-project-root)
                            'error nil)
                          nil nil))))
  (let ((ivy-file (expand-file-name "ivy.xml" ivy-dir))
        xml-root)
    (when (file-exists-p ivy-file)
      (setq xml-root (xml-parse-file ivy-file))
      (when xml-root
        (let ((dep-nodes (xml+-query-all xml-root '((dependencies) (dependency)))))
          (dolist (node dep-nodes)
          (print node)
            )
          )))))

(defun ensime-generate-for-jar-list (prj-dir)
  "Create .ensime file for project by jar-list.txt.

The jar-list.txt file contains dependencies jar files. We can get it from eclipse or intellij."
  (interactive
   (list (directory-file-name (read-directory-name "The project directory: "
                                                   (condition-case nil (projectile-project-root) 'error nil)
                                                   nil nil))))
  (let ((jar-list-file (expand-file-name "jar-list.txt" prj-dir))
        jar-list ensime-content)
    (if (not (file-exists-p jar-list-file))
        (message "The jar-list.txt file doesn't exist in the directory.")
      (with-temp-buffer
        (insert-file-contents-literally jar-list-file)
        (beginning-of-buffer) (replace-string "\\\\" "/")
        (beginning-of-buffer) (replace-string "\\" "/")
        (beginning-of-buffer) (replace-regexp "^" "\"")
        (beginning-of-buffer) (replace-regexp "$" "\"")
        (delete-blank-lines)
        (setq jar-list (buffer-string)))
      (with-temp-buffer
        (insert-file-contents-literally ensime-generate-template)
        (setq ensime-content (buffer-string)))
      (with-temp-file (expand-file-name ".ensime" prj-dir)
        (insert-string ensime-content)
        (beginning-of-buffer) (replace-string "@prj-dir@"    prj-dir)
        (beginning-of-buffer) (replace-string "@prj-name@"   (file-name-directory prj-dir))
        (beginning-of-buffer) (replace-string "@java-home@"  ensime-generate-jdk8)
        (beginning-of-buffer) (replace-string "@maven-repo@" ensime-generate-maven-repo)
        (beginning-of-buffer) (replace-string "@maven-root@" ensime-generate-maven-root)
        (beginning-of-buffer) (replace-string "@jar-list@"   jar-list)
        ))))

(provide 'ensime-generate)
;;; ensime-generate.el ends here
