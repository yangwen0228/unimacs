;;; init-python-mode.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package python
  :ensure nil
  :mode (("SConstruct\\'" . python-mode)
         ("SConscript\\'" . python-mode)
         ("\\.py\\'"      . python-mode))
  :config
  (autoload 'doctest-mode "doctest-mode" "Python doctest editing mode." t)
  (setq python-shell-interpreter "python")
  (add-to-list 'interpreter-mode-alist '("python" . python-mode))
  )

(provide 'init-python-mode)
;;; init-python-mode.el ends here
