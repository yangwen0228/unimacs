;;; init-emacs.el --- init all the variables in the origin emacs
;;; Commentary:
;; init all the variables in the origin emacs

;;; Code:

(defun add-fun-to-hooks (fun hooks)
  "Add function to hooks"
  (dolist (hook hooks)
    (add-hook hook fun)))

(defun add-funs-to-hook (funs hook)
  "Add list of functions to hook."
  (dolist (fun funs)
    (add-hook hook fun)))

;; define a key map, must define a interactive function. turn the function to command.
;; quit the help window.
(defun my-quit-help-window ()
  (interactive)
  (quit-windows-on "*Help*" t))

(define-key lisp-mode-map (kbd "C-M-q") 'my-quit-help-window)
(define-key emacs-lisp-mode-map (kbd "C-M-q") 'my-quit-help-window)

(provide 'init-emacs)
;;; init-emacs.el ends here
