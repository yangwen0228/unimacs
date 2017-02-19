;;; init-helm-flycheck.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package helm-flycheck
  :bind ("C-c ! h" . helm-flycheck)
  :config
  (use-package flycheck
    :init
    (flycheck-mode t)
    ;; Override default flycheck triggers
    (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
          flycheck-idle-change-delay 2)

    ;; Use pos-tip to show the error messages
    (use-package flycheck-pos-tip)
    (require 'popup)

    ;; Use my message function to show the error messages
    (defun my-format-message (message)
      (format "%s%s" "* " message))
    (defun flycheck-pos-tip-error-messages-1 (errors)
      "Display the tooltip that the messages of ERRORS.

Concatenate all non-nil messages of ERRORS separated by empty
lines, and display them with `pos-tip-show-no-propertize', which shows
 the messages in tooltip, depending on the number of lines."
      (-when-let (messages (remove-duplicates (-keep #'flycheck-error-message errors) :test 'equal))
        ;; (setq messages (remove-duplicates messages))
        ;; (popup-tip (mapconcat 'identity messages "\n"))
        (popup-tip (mapconcat 'my-format-message messages "\n"))
        ))

    (eval-after-load 'flycheck
      '(custom-set-variables
        '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages-1)))


    ;; flycheck for tcl, need ActiveTcl Kit.
    ;; (require 'flycheck-tcl)

    ;; flycheck for tcl, by tclpro1.4
    (cond
     (*win32*
      (flycheck-def-option-var flycheck-tclchecker-use-packages nil tclchecker
        "A list of specific Tcl packages to check with `-use'.

The value of this variable is a list of strings, where each
string is a package name with an optional version number attached such as `Tcl' or `Tcl8.6'."
        :type '(repeat (string :tag "Package name (optionally with version)"))
        :safe #'flycheck-string-list-p
        :package-version '(flycheck . "0.25"))

      (flycheck-define-checker tclchecker
        "A Tcl checker using tclpro1.4's procheck."
        ;; :command ("C:\\Program Files (x86)\\TclPro1.4\\win32-ix86\\bin\\procheck" "-quiet" "-W2" (option-list "-use" flycheck-tclchecker-use-packages) source)
        :command ("C:\\TclDevKit\\bin\\tclchecker.exe" "-quiet" "-W3" "-pcx" "C:\\TclDevKit\\pcx" "-use" "Tk" (option-list "-use" flycheck-tclchecker-use-packages) source)
        ;; :command ("d:\\tcl\\kotcllint.exe" "-quiet" "-Wall" "-pcx" "C:\\TclDevKit\\pcx" "-use" "Tk" (option-list "-use" flycheck-tclchecker-use-packages) source)
        ;; :command ("ttclcheck.exe" -r -log warn -scan source)
        :error-patterns
        ((warning line-start (file-name) ":" line " (warn" (one-or-more (any alpha)) ") " (message) line-end)
         (error line-start (file-name) ":" line " (" (one-or-more (any alpha)) ") " (message) line-end))
        :modes tcl-mode)
      (add-to-list 'flycheck-checkers 'tclchecker)
      )
     (*is-a-mac*
      (flycheck-def-option-var flycheck-tclchecker-use-packages nil tclchecker
        "A list of specific Tcl packages to check with `-use'.

The value of this variable is a list of strings, where each
string is a package name with an optional version number attached such as `Tcl' or `Tcl8.6'."
        :type '(repeat (string :tag "Package name (optionally with version)"))
        :safe #'flycheck-string-list-p
        :package-version '(flycheck . "0.17"))

      (flycheck-define-checker tclchecker
        "A Tcl checker using tclpro1.4's procheck."
        ;; :command ("/Applications/Komodo IDE 8.app/Contents/SharedSupport/tcl/kotcllint" "-quiet" "-W2" (option-list "-use" flycheck-tclchecker-use-packages) source)
        :command ("tclchecker" "-quiet" "-W3" (option-list "-use" flycheck-tclchecker-use-packages) source)
        :error-patterns
        ((warning line-start (file-name) ":" line " (warn" (one-or-more (any alpha)) ") " (message) line-end)
         (error line-start (file-name) ":" line " (" (one-or-more (any alpha)) ") " (message) line-end))
        :modes tcl-mode)
      (add-to-list 'flycheck-checkers 'tclchecker)
      ))
    (setq flycheck-tclchecker-use-packages '("Tk" "treectrl"))
    (defun flycheck-fresh-tclchecker ()
      (interactive)
      (dolist (filename (directory-files (expand-file-name "~/Library/Application Support/ActiveState/") t ".inf"))
        (delete-file filename t)))

    ;; Use clang to check c/c++
    ;; (require 'init-clang)

    ;; (defun my-flycheck-clang-init ()
    ;;   (setq flycheck-clang-include-path (append '(".") my-clang-include-directories))
    ;;   )

    ;; (add-hook 'c-mode-common-hook 'my-flycheck-clang-init)

    (defun my-flycheck-irony-init ()
      (require 'flycheck-irony)
      (set (make-local-variable 'flycheck-checkers) nil)
      (flycheck-irony-setup)
      )

    (add-hook 'c-mode-hook    'my-flycheck-irony-init)
    (add-hook 'c++-mode-hook  'my-flycheck-irony-init)
    (add-hook 'objc-mode-hook 'my-flycheck-irony-init)

    ;; (eval-after-load 'flycheck
    ;;   '(progn
    ;;      (require 'flycheck-google-cpplint)
    ;;      ;; Add Google C++ Style checker.
    ;;      ;; In default, syntax checked by Clang and Cppcheck.
    ;;      (flycheck-add-next-checker 'c/c++-clang
    ;;                                 'c/c++-googlelint 'append)))
    ))

(provide 'init-helm-flycheck)
;;; init-helm-flycheck.el ends here
