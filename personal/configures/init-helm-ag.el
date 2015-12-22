;;; init-helm-ag.el --- Helm ag configure.
;;; Commentary:
;; comments
;; Please add following configuration if you use helm-ag with the platinum searcher.

;; (custom-set-variables
;;   '(helm-ag-base-command "pt --nocolor --nogroup"))

;; or using ack

;; (custom-set-variables
;;   '(helm-ag-base-command "ack --nocolor --nogroup"))

;;; Code:
(custom-set-variables
 '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
 '(helm-ag-command-option "--all-text")
 '(helm-ag-insert-at-point 'symbol))

(defun projectile-helm-ag ()
  (interactive)
  (helm-ag (projectile-project-root)))

;; helm-ag input search directory.
(defun helm-ag-select-directory ()
  "Like `helm-ag', but ag from selected dir."
  (interactive)
  (let ((default-directory
          (file-name-as-directory (read-directory-name "Search directory: " nil nil t))))
    (helm-ag default-directory)
    ))
(global-set-key (kbd "C-c M-c") 'helm-ag)
(global-set-key (kbd "C-c M-r") 'helm-ag-select-directory)

(provide 'init-helm-ag)
;;; init-helm-ag.el ends here