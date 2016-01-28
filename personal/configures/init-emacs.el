;;; init-emacs.el --- init all the variables in the origin emacs
;;; Commentary:
;; init all the variables in the origin emacs

;;; Code:

(defsubst add-fun-to-hooks (fun hooks)
  "Add function to hooks"
  (dolist (hook hooks)
    (add-hook hook fun)))

(defsubst add-funs-to-hook (funs hook)
  "Add list of functions to hook."
  (dolist (fun funs)
    (add-hook hook fun)))

;; define a key map, must define a interactive function. turn the function to command.
;; quit the help window.
(defun my-quit-help-window ()
  (interactive)
  (quit-windows-on "*Help*" t))

(define-key lisp-mode-map       (kbd "C-M-q") 'my-quit-help-window)
(define-key emacs-lisp-mode-map (kbd "C-M-q") 'my-quit-help-window)

(setq browse-url-generic-program "C:/Program Files (x86)/Mozilla Firefox/firefox.exe")
;; (setq browse-url-generic-program "C:/Users/yangwen/AppData/Local/Google/Chrome/Application/chrome.exe")
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (browse-url-generic (concat "file://" (buffer-file-name))))

;; Chinese in windows system.
(when (eq system-type 'windows-nt)
  (set-default 'process-coding-system-alist
               '(("find"   gbk-dos . gbk-dos)
                 ("global" gbk-dos . gbk-dos)
                 ("gtags"  gbk-dos . gbk-dos)
                 ("ctags"  gbk-dos . gbk-dos)
                 ("ag"     gbk-dos . gbk-dos)
                 )))

(provide 'init-emacs)
;;; init-emacs.el ends here
