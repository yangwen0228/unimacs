(require 'python)
(autoload 'doctest-mode "doctest-mode" "Python doctest editing mode." t)
(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))
(setq python-shell-interpreter "python")

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; company jedi
(require 'company-jedi)

;; Standard Jedi.el setting
(setq jedi:mode-function 'jedi:get-in-function-call-when-idle)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

(provide 'init-python-mode)
