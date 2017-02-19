;;; init-company.el --- Summary:
;; This file is used for configure the company-mode;

;;; Commentary:
;; The company-mode is much better than auto-complete-mode.
;; More information please see:
;; @https://github.com/company-mode/company-mode/issues/68
;; TODO: <tab> conflict with yasnippet
;;; Code:
(use-package company
  :defer 0
  :preface
  (defun define-company-backends (list)
    (dolist (cons list)
      (unimacs-company-define-backends cons)))
  (defun unimacs-company-define-backends (modes-backends-cons)
    (let ((modes    (car modes-backends-cons))
          (backends (cdr modes-backends-cons)))
      (dolist (mode modes)
        (let* ((modename (symbol-name mode))
               (funcname (concat "company-backends-for-" modename))
               (func (intern funcname))
               (hook (intern (concat modename "-hook"))))
          (setf (symbol-function func)
                `(lambda ()
                   (set (make-local-variable 'company-backends)
                        ',backends)))
          (add-hook hook func)))))
  :config
  (global-company-mode t)
  (define-company-backends
    '(((c-mode c++-mode objc-mode) . ((company-irony company-dabbrev-code) company-c-headers))
      ;; ((js-mode js2-mode)          . (company-tern))
      ((web-mode)                  . (company-web-html company-dabbrev-code company-css company-files))
      ))

  (setq company-idle-delay            0.1
        company-tooltip-limit         15
        company-minimum-prefix-length 2
        company-dabbrev-downcase      nil ; not downcase.
        company-dabbrev-ignore-case   nil ; not downcase.
        company-require-match         nil
        company-show-numbers          t)

  (define-key company-active-map [tab] nil)
  (define-key company-active-map (kbd "C-j") 'company-show-location)
  (define-key company-search-map (kbd "C-j") 'company-show-location)
  )

;; (use-package company-tern
;;   :commands (company-tern company-tern-create-project)
;;   :preface
;;   (defun company-tern-create-project (dir)
;;     (interactive "D")
;;     (let ((file (expand-file-name ".tern-project" dir)))
;;       (with-temp-file file
;;         (erase-buffer)
;;         (insert "
;; {
;; \"libs\": [
;;          \"browser\",
;;          \"jquery\"
;;          ],
;; \"loadEagerly\": [
;;                 \"jquery-*.min.js\"
;;                 ],
;; \"dontLoad\": [
;;              ],
;; \"plugins\": {
;; \"node\": {}
;; }
;; }
;; "
;;                        ))))
;;   :init
;;   (use-package tern
;;     :init
;;     ;; (setq tern-command '("node" "/path/to/npm/node_modules/tern/bin/tern"))
;;     (setq tern-command (cons (executable-find "tern") '())) ; good solution.
;;     (add-hook 'js2-mode-hook 'tern-mode)
;;     (add-hook 'web-mode-hook 'tern-mode)))

(use-package company-quickhelp
  :disabled t
  :init (company-quickhelp-mode 1))

(use-package company-clang
  ;; Configure for company-clang:
  :disabled t ; use company-irony instead.
  )

(use-package company-irony
  ;; Configure for company-irony-mode:
  :commands (company-irony)
  :config
  (require 'init-clang)
  (setq company-clang-arguments
        (mapcar (lambda (item) (concat "-I" item))
                my-clang-include-directories))
  ;; don't use (require 'irony). Because company-irony already require it.
  ;; (optional) adds CC special commands to `company-begin-commands' in order to
  ;; trigger completion at interesting places, such as after scope operator
  ;;     std::|
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))

(use-package company-c-headers
  ;; Configure for company-c-headers:
  :commands (company-c-headers)
  :config
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
      )))

(provide 'init-company)
;;; init-company.el ends here
