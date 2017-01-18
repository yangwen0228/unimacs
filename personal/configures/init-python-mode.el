;;; init-python-mode.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package python-mode
  :mode (("SConstruct\\'" . python-mode)
         ("SConscript\\'" . python-mode)
         ("\\.py\\'"      . python-mode))
  :config
  (use-package anaconda-mode
    :disabled t
    :config
    (use-package company-anaconda)
    )
  (use-package jedi
    :disabled
    :config
    (use-package jedi-core)
    (use-package company-jedi
  :commands (company-jedi)
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  ;; Standard Jedi.el setting
  (setq jedi:mode-function 'jedi:get-in-function-call-when-idle)
  (setq jedi:setup-keys t)
  (setq jedi:complete-on-dot t)))
  
  (use-package elpy
    :init
    (elpy-enable)
    (elpy-use-ipython)
    ;; use flycheck not flymake with elpy
    (when (require 'flycheck nil t)
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
      (add-hook 'elpy-mode-hook 'flycheck-mode)))
  (autoload 'doctest-mode "doctest-mode" "Python doctest editing mode." t)
  (setq python-shell-interpreter "python")
  (add-to-list 'interpreter-mode-alist '("python" . python-mode))
  )

(provide 'init-python-mode)
;;; init-python-mode.el ends here
