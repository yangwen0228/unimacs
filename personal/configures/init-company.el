;;; init-company.el --- Summary:
;; This file is used for configure the company-mode;

;;; Commentary:
;; The company-mode is much better than auto-complete-mode.
;; More information please see:
;; @https://github.com/company-mode/company-mode/issues/68

;;; Code:
(require 'company)
(require 'company-quickhelp)
(company-quickhelp-mode 1)
(defun my-cc-mode-company-backends-init ()
  (set (make-local-variable 'company-backends)
       '(company-c-headers
         company-irony
         company-dabbrev
         company-capf)))
(add-to-hooks 'my-cc-mode-company-backends-init
              '(c-mode-hook c++-mode-hook objc-mode-hook))

(defun my-tcl-mode-company-backends-init ()
  (set (make-local-variable 'company-backends)
       '((company-dabbrev-code
          company-gtags
          company-etags
          company-keywords)
         company-dabbrev company-files company-capf))
  (add-to-list 'company-keywords-alist (append '(tcl-mode) tcl-hm-commands-list))
  )
(add-to-hooks 'my-tcl-mode-company-backends-init
              '(tcl-hm-mode-hook tcl-mode-hook))

(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.1)
(setq company-tooltip-limit 15)
(setq company-minimum-prefix-length 2)
(setq company-dabbrev-downcase nil)    ; Don't use downcase when complete in the comment or string.
(setq company-require-match nil)

;; Configure for company-clang:
(require 'init-clang)
(setq company-clang-arguments
      (mapcar (lambda (item) (concat "-I" item))
              my-clang-include-directories))

(if (fboundp 'evil-declare-change-repeat)
    (mapc #'evil-declare-change-repeat
          '(company-complete-common
            company-select-next
            company-select-previous
            company-complete-selection
            )))


;; Configure for company-irony-mode:
;; (require 'company-irony)
;; ;; (optional) adds CC special commands to `company-begin-commands' in order to
;; ;; trigger completion at interesting places, such as after scope operator
;; ;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;; Configure for company-c-headers:
(require 'company-c-headers)
(defun get-include-paths-by-irony-cdb ()
  (let ((options (caar (irony-cdb-clang-complete--get-compile-options)))
        (dir (file-name-directory (irony-cdb-clang-complete--locate-db)))
        (paths))
    (mapcar #'(lambda (option)
                  (if (equal "-include" (substring option 0 (min 8 (length option))))
                      (setq paths (append paths (list (expand-file-name (substring option 9) dir))))
                    (setq paths (append paths (list (expand-file-name (substring option 2) dir))))
                    )) options)
    paths))
;; You will probably want to customize the `company-c-headers-path-user' and
;; `company-c-headers-path-system' variables for your specific needs.
;; (setq company-c-headers-path-system my-clang-include-directories)
(defadvice company-c-headers (before company-c-headers activate)
    "Update the include paths when completing."
    (setq company-c-headers-path-system (get-include-paths-by-irony-cdb))
    )

;; Redefine the origin function `company-c-headers--candidates',
;; in order not to show the system headers when input user headers.
(defun company-c-headers--candidates-fail (prefix)
  "Return candidates for PREFIX."
  (let ((p (if (equal (aref prefix 0) ?\")
               (call-if-function company-c-headers-path-user)
             (call-if-function company-c-headers-path-system)))
        (next (when (equal (aref prefix 0) ?\")
                ;; (call-if-function company-c-headers-path-system)
                nil ;; Don't show the system candidates when input user headers.
                ))
        candidates)
    (while p
      (when (file-directory-p (car p))
        (setq candidates (append candidates (company-c-headers--candidates-for prefix (car p)))))

      (setq p (or (cdr p)
                  (let ((tmp next))
                    (setq next nil)
                    tmp)))
      )
    candidates
    ))

(provide 'init-company)

;;; init-company.el ends here
