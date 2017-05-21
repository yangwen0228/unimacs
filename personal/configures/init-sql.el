;;; init-sql.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package sql :ensure nil
  :mode ("\\.sql\\'" . sql-mode)
  :commands (sql-mode sql-mysql)
  :config
  (use-package sql-indent)
  (use-package sqlup-mode
    :init (sqlup-mode +1))

  (setq sql-mysql-program "c:/Program Files/MySQL/MySQL Server 5.7/bin/mysql.exe")
  (setq sql-mysql-options '("-C" "-f" "-t" "-n")) ; for windows

  (setq sql-mysql-login-params
        '((user :default "root")
          (database)
          (server :default "localhost")
          (port :default 3306)))

;;; hooks
  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (toggle-truncate-lines t)
              (setq-local show-trailing-whitespace nil)))

  (add-hook 'sql-mode-hook
            (lambda ()
              (setq-local ac-ignore-case t)))

;;; server list
  (setq sql-connection-alist
        '((local.mysql (sql-product 'mysql)
                       (sql-port 3306)
                       (sql-server "localhost")
                       (sql-user "root")
                       (sql-database "test"))
          ))

;;; TODO update this function
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
