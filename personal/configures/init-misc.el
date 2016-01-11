;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------

;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(add-hook 'comint-mode-hook
          (lambda () (setq show-trailing-whitespace nil)))

;; turns on auto-fill-mode, don't use text-mode-hook because for some
;; mode (org-mode for example), this will make the exported document
;; ugly!
;; (add-hook 'markdown-mode-hook 'turn-on-auto-fill)
;; (add-hook 'change-log-mode-hook 'turn-on-auto-fill)
;; (add-hook 'cc-mode-hook 'turn-on-auto-fill)
;; (global-set-key (kbd "C-c q") 'auto-fill-mode)

;;----------------------------------------------------------------------------
;; Misc config - yet to be placed in separate files
;;----------------------------------------------------------------------------
;; {{ shell and conf
(add-to-list 'auto-mode-alist '("\\.[a-zA-Z]+rc$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.meta\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc\\'" . sh-mode))
;; }}

;; Write backup files to own directory
;; (if (not (file-exists-p "~/.emacs.d/.backups"))
;;     (make-directory (expand-file-name "~/.emacs.d/.backups")))
;; (setq
;;   backup-by-coping t ; don't clobber symlinks
;;   backup-directory-alist '(("." . "~/.emacs.d/.backups"))
;;   delete-old-versions t
;;   kept-new-versions 6
;;   kept-old-versions 2
;;   version-control t  ;use versioned backups
;;   )
;; Make backups of files, even when they're in version control
(setq vc-make-backup-files nil)

                                        ; from RobinH
                                        ;Time management
;; (setq display-time-24hr-format t)
;; (setq display-time-day-and-date t)
;; (display-time)

                                        ;effective emacs item 3
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\M-s" 'isearch-backward-regexp)
(global-set-key "\C-\M-s" 'tags-search)
(global-set-key "\C-x\C-n" 'find-file-other-frame) ;open new frame with a file

;;a no-op function to bind to if you want to set a keystroke to null
(defun void () "this is a no-op" (interactive))

                                        ;show ascii table
(defun ascii-table ()
  "Print the ascii table. Based on a defun by Alex Schroeder <asc@bsiag.com>"
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (let ((i 0))
    (while (< i 254)
      (setq i (+ i 1))
      (insert (format "%4d %c\n" i i))))
  (beginning-of-buffer))

;; @see http://www.emacswiki.org/emacs/BetterRegisters
;; This is used in the function below to make marked points visible
(defface register-marker-face '((t (:background "grey")))
         "Used to mark register positions in a buffer."
         :group 'faces)

(defun open-readme-in-git-root-directory ()
  (interactive)
  (let (filename
        (root-dir (locate-dominating-file (file-name-as-directory (file-name-directory buffer-file-name)) ".git"))
        )
    ;; (message "root-dir=%s" root-dir)
    (and root-dir (file-name-as-directory root-dir))
    (setq filename (concat root-dir "README.org"))
    (if (not (file-exists-p filename))
        (setq filename (concat root-dir "README.md"))
      )
    ;; (message "filename=%s" filename)
    (if (file-exists-p filename)
        (switch-to-buffer (find-file-noselect filename nil nil))
      (message "NO README.org or README.md found!"))
    ))
(global-set-key (kbd "C-c C-q") 'open-readme-in-git-root-directory)

;; {{ eval and replace anywhere
;; @see http://emacs.wordpress.com/2007/01/17/eval-and-replace-anywhere/ 
(defun fc-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(global-set-key (kbd "C-c e") 'fc-eval-and-replace)

(defun calc-eval-and-insert (&optional start end)
  (interactive "r")
  (let ((result (calc-eval (buffer-substring-no-properties start end))))
    (goto-char (point-at-eol))
    (insert " = " result)))

(defun calc-eval-line-and-insert ()
  (interactive)
  (calc-eval-and-insert (point-at-bol) (point-at-eol)))

;; }}

;; input open source license
(require 'legalese)

;; {{string-edit.el
(autoload 'string-edit-at-point "string-edit" "enable string-edit-mode" t)
;; }}

;; {{ issue-tracker
(global-set-key (kbd "C-c C-t") 'issue-tracker-increment-issue-id-under-cursor)
;; }}

;; {{ show email sent by `git send-email' in gnus
(require 'gnus-article-treat-patch)
(setq gnus-article-patch-conditions
      '( "^@@ -[0-9]+,[0-9]+ \\+[0-9]+,[0-9]+ @@" ))
;; }}

(defun convert-image-to-css-code ()
  "convert a image into css code (base64 encode)"
  (interactive)
  (let (str
        rlt
        (file (read-file-name "The path of image:"))
        )
    (with-temp-buffer
      (shell-command (concat "cat " file "|base64") 1)
      (setq str (replace-regexp-in-string "\n" "" (buffer-string)))
      )
    (setq rlt (concat "background:url(data:image/"
                      (car (last (split-string file "\\.")))
                      ";base64,"
                      str
                      ") no-repeat 0 0;"
                      ))
    (kill-new rlt)
    (message "css code => clipboard & yank ring")))

(defun current-font-face ()
  "get the font face under cursor"
  (interactive)
  (let ((rlt (format "%S" (get-text-property (- (point) 1) 'face))))
    (kill-new rlt)
    (message "%s => clipboard & yank ring" rlt)))

(defun add-pwd-into-load-path ()
  "add current directory into load-path, useful for elisp developers"
  (interactive)
  (let ((dir (expand-file-name default-directory)))
    (if (not (memq dir load-path))
        (add-to-list 'load-path dir))
    (message "Directory added into load-path:%s" dir)))

(setq system-time-locale "C")

;; {{ unique lines
(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-cha

             r start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))
;; }}

;; {{start dictionary lookup
;; use below commands to create dicitonary
;; mkdir -p ~/.stardict/dic
;; # wordnet English => English
;; curl http://abloz.com/huzheng/stardict-dic/dict.org/stardict-dictd_www.dict.org_wn-2.4.2.tar.bz2 | tar jx -C ~/.stardict/dic
;; # Langdao Chinese => English
;; curl http://abloz.com/huzheng/stardict-dic/zh_CN/stardict-langdao-ec-gb-2.4.2.tar.bz2 | tar jx -C ~/.stardict/dic
;;

;; (setq sdcv-dictionary-simple-list '("朗道英汉字典5.0"))
;; (setq sdcv-dictionary-complete-list '("WordNet"))
;; (autoload 'sdcv-search-pointer "sdcv" "show word explanation in buffer" t)
;; (autoload 'sdcv-search-input+ "sdcv" "show word explanation in tooltip" t)
;; (global-set-key (kbd "C-c ; b") 'sdcv-search-pointer)
;; (global-set-key (kbd "C-c ; t") 'sdcv-search-input+)
;; }}

(setq web-mode-imenu-regexp-list
      '(;; ("<\\(h[1-9]\\)\\([^>]*\\)>\\([^<]*\\)" 1 3 ">" nil)
        ("^[ \t]*<\\([@a-z]+\\)[^>]*>? *$" 1 " id=\"\\([a-zA-Z0-9_]+\\)\"" "#" ">")
        ("^[ \t]*<\\(@[a-z.]+\\)[^>]*>? *$" 1 " contentId=\"\\([a-zA-Z0-9_]+\\)\"" "=" ">")))

;; {{ imenu
(setq imenu-max-item-length 128)
(setq imenu-max-item-length 64)
;; }}


(defun toggle-web-js-offset ()
  "toggle js2-basic-offset"
  (interactive)
  (let ((v (if (= js2-basic-offset 2) 4 2)))
    (setq web-mode-indent-style v)
    (setq web-mode-code-indent-offset v)
    (setq web-mode-css-indent-offset v)
    (setq web-mode-markup-indent-offset v)
    (setq js2-basic-offset v)
    (message "web-mode js2-mode indent=%d" v)))

(provide 'init-misc)
