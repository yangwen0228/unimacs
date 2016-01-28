;;; init-company.el --- Summary:
;; This file is used for configure the company-mode;

;;; Commentary:
;; The company-mode is much better than auto-complete-mode.
;; More information please see:
;; @https://github.com/company-mode/company-mode/issues/68
;; TODO: <tab> conflict with yasnippet
;;; Code:
(use-package company
  :preface
  (defun define-company-backends (list)
    (dolist (cons list)
      (let ((modes    (car cons))
            (backends (cdr cons)))
        (dolist (mode modes)
          (let* ((modename (symbol-name mode))
                 (funcname (concat "company-backends-for-" modename))
                 (func (intern funcname))
                 (hook (intern (concat modename "-hook"))))
            (setf (symbol-function func)
                  `(lambda ()
                     (set (make-local-variable 'company-backends)
                          ',backends)))
            (add-hook hook func))))))
  :init
  (global-company-mode t)
  (define-company-backends
    '(((c-mode c++-mode objc-mode) . ((company-irony company-dabbrev-code) company-c-headers))
      ((tcl-hm-mode tcl-mode)      . ((company-keywords company-gtags-tcl-rigid company-dabbrev-code) company-files))
      ((js-mode js2-mode)          . (company-tern))
      ((web-mode)                  . (company-web-html company-tern company-dabbrev-code company-css company-files))
      ))

  (setq company-idle-delay            0.1
        company-tooltip-limit         15
        company-minimum-prefix-length 2
        company-dabbrev-downcase      nil ; not downcase.
        company-require-match         nil
        company-show-numbers          t)

  (require 'tcl-hm-mode)
  (add-to-list 'company-keywords-alist
               (append '(tcl-mode) tcl-hm-commands-list))

  (define-key company-active-map [tab] nil)
  (define-key company-active-map (kbd "C-j") 'company-show-location)
  ;; (define-key company-active-map (kbd "C-n") 'company-select-next)
  ;; (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-j") 'company-show-location)
  ;; (define-key company-search-map (kbd "C-n") 'company-select-next)
  ;; (define-key company-search-map (kbd "C-p") 'company-select-previous)
  )

(use-package company-tern
  :commands (company-tern company-tern-create-project)
  :preface
  (defun company-tern-create-project (dir)
    (interactive "D")
    (let ((file (expand-file-name ".tern-project" dir)))
      (with-temp-file file
        (erase-buffer)
        (insert-string "
{
\"libs\": [
         \"browser\",
         \"jquery\"
         ],
\"loadEagerly\": [
                \"jquery-*.min.js\"
                ],
\"dontLoad\": [
             ],
\"plugins\": {
\"node\": {}
}
}
"
                       ))))
  :init
  (use-package tern
    :init
    ;; (setq tern-command '("node" "/path/to/npm/node_modules/tern/bin/tern"))
    (setq tern-command (cons (executable-find "tern") '())) ; good solution.
    (add-hook 'js2-mode-hook 'tern-mode)
    (add-hook 'web-mode-hook 'tern-mode)))

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

(use-package company-gtags
  :ensure nil
  :commands (company-gtags company-gtags-tcl-rigid)
  :preface
  (defun company-gtags--fetch-tcl-tags-rigid (prefix)
    (with-temp-buffer
      (let (tags)
        (when (= 0 (call-process company-gtags-executable nil
                                 (list (current-buffer) nil) nil "-xgq" (concat "" prefix)))
          ;; (print (buffer-string))
          (goto-char (point-min))
          (cl-loop while
                   (re-search-forward (concat
                                       "^" prefix ; echo pattern
                                       "[ \t]+\\([[:digit:]]+\\)" ; linum
                                       "[ \t]+\\([^ \t]+\\)" ;; file
                                       "[ \t]*\\(proc[ \t]+\\|.*?\\[\\)\\(::\\)?\\([a-zA-Z0-9:._-]+::\\)*?" ; filter
                                       "\\(" prefix "[a-zA-Z0-9:._-]*\\)" ; completion
                                       "\\(.*\\)" ; definition
                                       ;; "[ \t]+\\(.*\\)" ; definition
                                       "$"
                                       ) nil t)
                   collect
                   (propertize (concat (match-string 4) (match-string 5) (match-string 6))
                               'meta (concat (match-string 3) (match-string 4) (match-string 5) (match-string 6) (match-string 7))
                               'location (cons (expand-file-name (match-string 2))
                                               (string-to-number (match-string 1)))))))))

  (defun company-gtags-tcl-rigid (command &optional arg &rest ignored)
    "Support for tcl ns::proc kind of command.  COMMAND ARG IGNORED."
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'company-gtags-tcl-rigid))
      (prefix (and company-gtags-executable
                   buffer-file-name
                   (apply #'derived-mode-p company-gtags-modes)
                   (not (company-in-string-or-comment))
                   (company-gtags--tags-available-p)
                   (or (company-grab-symbol) 'stop)))
      (candidates (company-gtags--fetch-tcl-tags-rigid arg))
      (sorted t)
      (duplicates t)
      (annotation (company-gtags--annotation arg))
      (meta (get-text-property 0 'meta arg))
      (location (get-text-property 0 'location arg))
      (post-completion (let ((anno (company-gtags--annotation arg)))
                         (when (and company-gtags-insert-arguments anno)
                           (insert anno)
                           (company-template-c-like-templatify anno)))))))

(use-package company-jedi
  :commands (company-jedi)
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  ;; Standard Jedi.el setting
  (setq jedi:mode-function 'jedi:get-in-function-call-when-idle)
  (setq jedi:setup-keys t)
  (setq jedi:complete-on-dot t))

(provide 'init-company)
;;; init-company.el ends here
