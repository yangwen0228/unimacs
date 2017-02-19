;;; init-python-mode.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package python-mode
  :mode (("SConstruct\\'" . python-mode)
         ("SConscript\\'" . python-mode)
         ("\\.py\\'"      . python-mode))
  :interpreter ("python" . python-mode)
  :config
  (setq py-shell-name nil)              ; Use the env path version
  ;; bug: workaround
  (defun py--buffer-filename-remote-maybe (&optional file-name buffer)
    (let ((file-name (or file-name (ignore-errors (if (file-readable-p (buffer-file-name)) (buffer-file-name) "")))))
      (if (and (featurep 'tramp) (tramp-tramp-file-p file-name))
          (tramp-file-name-localname
           (tramp-dissect-file-name file-name))
        file-name)))

  (use-package anaconda-mode :disabled
    :config
    (use-package company-anaconda))
  (use-package jedi :disabled
    :config
    (use-package jedi-core)
    (use-package company-jedi
      :config
      (add-hook 'python-mode-hook 'jedi:setup)
      ;; Standard Jedi.el setting
      (setq jedi:mode-function 'jedi:get-in-function-call-when-idle)
      (setq jedi:setup-keys t)
      (setq jedi:complete-on-dot t)))

  (use-package elpy
    :config
    (elpy-enable)
    (elpy-use-ipython)
    (setq python-shell-completion-native-enable nil)
    ;; use flycheck not flymake with elpy
    (when (require 'flycheck nil t)
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
      (add-hook 'elpy-mode-hook 'flycheck-mode)))
  )

(provide 'init-python-mode)
;;; init-python-mode.el ends here
