;;; init-yasnippet.el --- Summary:
;;; Commentary:
;;; code:
(use-package yasnippet
  :commands yas-global-mode
  :init (add-hook 'prog-mode-hook (lambda () (yas-global-mode 1)))
  :config
  ;; fix conflict where smartparens clobbers yas' key bindings
  (defadvice yas-expand (before dotemacs activate)
    (sp-remove-active-pair-overlay))
  ;; (add-hook 'yas-before-expand-snippet-hook (lambda () (smartparens-mode -1)))
  ;; (add-hook 'yas-after-exit-snippet-hook    (lambda () (smartparens-mode 1)))

  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   ;; '(yas-field-highlight-face ((t (:inherit secondary-selection :background "gray" :foreground "black"))))
   '(yas-field-highlight-face ((t (:inherit secondary-selection)))))

  (setq unimacs-yasnippet-dir (expand-file-name "snippets" unimacs-utils-dir))
  (setq yas-snippet-dirs (list yas-installed-snippets-dir))
  (when (and (file-exists-p unimacs-yasnippet-dir)
             (not (member unimacs-yasnippet-dir yas-snippet-dirs)))
    (add-to-list 'yas-snippet-dirs unimacs-yasnippet-dir))
  ;; give yas-dropdown-prompt in yas-prompt-functions a chance.(others: yas-ido-prompt yas-completing-prompt)
  (use-package dropdown-list)
  (setq yas-prompt-functions '(yas-dropdown-prompt yas-completing-prompt))
  ;; use yas-completing-prompt when ONLY when `M-x yas-insert-snippet'
  ;; thanks to capitaomorte for providing the trick.
  (defadvice yas-insert-snippet (around use-completing-prompt activate)
    "Use `yas-completing-prompt' for `yas-prompt-functions' but only here..."
    (let ((yas-prompt-functions '(yas-completing-prompt)))
      ad-do-it))
  ;; @see http://stackoverflow.com/questions/7619640/emacs-latex-yasnippet-why-are-newlines-inserted-after-a-snippet
  (setq-default mode-require-final-newline nil)
  :diminish (yas-minor-mode yas-global-mode)
  )

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
