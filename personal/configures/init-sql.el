;;; init-sql.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package sql :ensure nil
  :mode ("\\.sql\\'" . sql-mode)
  :commands (sql-mode sql-mysql)
  :init (add-hook 'sql-mode-hook 'sqlup-mode)
  :config
  (defun company-sql (command &optional arg &rest ignored)
    "A `company-mode' completion back-end for interactive sql."
    (interactive (list 'interactive))
    (case command
      ('interactive (company-begin-backend 'company-sql))
      ('prefix (and (derived-mode-p 'sql-interactive-mode)
                    (not (company-in-string-or-comment))
                    (company-sql--grab-symbol)))
      ('candidates (sql-mysql-complete-command-company arg))))

  (defun company-sql--grab-symbol ()
    (let ((symbol (company-grab-symbol)))
      (when symbol
        (cons symbol
              (save-excursion
                (let ((pos (point)))
                  (goto-char (- (point) (length symbol)))
                  (while (eq (char-before) ?.)
                    (goto-char (1- (point)))
                    (skip-syntax-backward "w_"))
                  (- pos (point))))))))

  ;; completion functions
  (defun sql-mysql-complete-command-company (opt)
    (when opt
      (all-completions (upcase opt) sql-mysql-command-alist)))

  (defun sql-mysql-complete-comopt ()
    (let ((opt (comint-match-partial-filename))
          (cmd (upcase (save-excursion
                         (comint-bol nil)
                         (skip-chars-forward " \t")
                         (current-word)))))
      (when opt
        (let ((success (let ((comint-completion-addsuffix nil))
                         (comint-dynamic-simple-complete
                          (upcase opt)
                          (assoc cmd sql-mysql-command-option-alist)))))
          (when success
            (upcase-region (save-excursion (backward-word) (point))
                           (point))
            (if (and (memq success '(sole shortest))
                     comint-completion-addsuffix)
                (insert " ")))
          success))))

  (unimacs-company-define-backends
   '((sql-mode) . (company-dabbrev :with company-yasnippet)))

  (use-package sql-indent)
  (use-package sqlup-mode)

  (setq sql-mysql-program "c:/Program Files/MySQL/MySQL Server 5.7/bin/mysql.exe")
  (setq sql-mysql-options '("-C" "-f" "-t" "-n")) ; for windows

  (setq sql-mysql-login-params
        '((user :default "root")
          (database)
          (password :default "123456")
          (server :default "localhost")
          (port :default 3306)))

  ;; hooks
  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (toggle-truncate-lines t)
              (setq-local show-trailing-whitespace nil)))

  (add-hook 'sql-mode-hook
            (lambda ()
              (setq-local ac-ignore-case t)))

  ;; server list
  (setq sql-connection-alist
        '((local.mysql (sql-product 'mysql)
                       (sql-port 3306)
                       (sql-server "localhost")
                       (sql-user "root")
                       (sql-database "test"))
          ))

  ;; TODO update this function
  (defun tmtxt/sql-connect-server (connection)
    "Connect to the input server using tmtxt/sql-servers-list"
    (interactive
     (helm-comp-read "Select server: " (mapcar (lambda (item)
                                                 (list
                                                  (symbol-name (nth 0 item))
                                                  (nth 0 item)))
                                               sql-connection-alist)))
    ;; password
    ;; (require 'tmtxt-identica "tmtxt-identica.el.gpg")
    ;; get the sql connection info and product from the sql-connection-alist
    (let* ((connection-info (assoc connection sql-connection-alist))
           (connection-product (nth 1 (nth 1 (assoc 'sql-product connection-info))))
           ;; (sql-password (nth 1 (assoc connection tmtxt-sql-password)))
           (sql-password "123456")
           )
      ;; delete the connection info from the sql-connection-alist
      (setq sql-connection-alist (assq-delete-all connection sql-connection-alist))
      ;; delete the old password from the connection-info
      (setq connection-info (assq-delete-all 'sql-password connection-info))
      ;; add the password to the connection-info
      (nconc connection-info `((sql-password ,sql-password)))
      ;; add back the connection info to the beginning of sql-connection-alist
      ;; (last used server will appear first for the next prompt)
      (add-to-list 'sql-connection-alist connection-info)
      ;; override the sql-product by the product of this connection
      (setq sql-product connection-product)
      ;; connect
      (if current-prefix-arg
          (sql-connect connection connection)
        (sql-connect connection))))

  (defun sanityinc/pop-to-sqli-buffer ()
    "Switch to the corresponding sqli buffer."
    (interactive)
    (if sql-buffer
        (progn
          (pop-to-buffer sql-buffer)
          (goto-char (point-max)))
      (sql-set-sqli-buffer)
      (when sql-buffer
        (sanityinc/pop-to-sqli-buffer))))

  (define-key sql-mode-map (kbd "C-c C-z") 'sanityinc/pop-to-sqli-buffer)
  )

(provide 'init-sql)
;;; init-sql.el ends here
