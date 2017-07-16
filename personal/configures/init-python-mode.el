;;; init-python-mode.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package python-mode
  :mode (("SConstruct\\'" . python-mode)
         ("SConscript\\'" . python-mode)
         ("\\.py\\'"      . python-mode))
  :interpreter (("python" . python-mode) ("python3" . python-mode))
  :config
  (setq py-shell-name nil)              ; Use the env path version

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
