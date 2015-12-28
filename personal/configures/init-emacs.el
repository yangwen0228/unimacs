;;; init-emacs.el --- init all the variables in the origin emacs
;;; Commentary:
;; init all the variables in the origin emacs

;;; Code:

;; Files:
(setq inhibit-default-init t) ; bug @ ido.el about 'seq
(setq make-backup-files nil)
;; Hide/Show:
(require 'hideshow)
(add-hook 'prog-mode-hook 'hs-minor-mode)

(require 'font-lock+)

;; define a key map, must define a interactive function. turn the function to command.
;; quit the help window.
(defun my-quit-help-window ()
  (interactive)
  (quit-windows-on "*Help*" t))

(define-key lisp-mode-map (kbd "C-M-q") 'my-quit-help-window)
(define-key emacs-lisp-mode-map (kbd "C-M-q") 'my-quit-help-window)

(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-l") 'find-library)

;; specify the encoding system
(cond
 (*win32*
  ;; (setq default-buffer-file-coding-system 'cp936-dos)
  ;; (prefer-coding-system 'cp936-dos))
  (setq default-buffer-file-coding-system 'utf-8-unix)
  (prefer-coding-system 'utf-8-unix)
  )
 (t
  (setq default-buffer-file-coding-system 'utf-8-unix)
  (prefer-coding-system 'utf-8-unix))
  )

(provide 'init-emacs)
;;; init-emacs.el ends here
