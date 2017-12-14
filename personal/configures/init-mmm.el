;;; init-mmm.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package mmm-mode
  ;; :hook ((web-mode . mmm-mode))
  :commands mmm-mode
  :config
  (defun mmm-indent-line-web-sql-submode ()
    (web-mode-propertize)
    (let (cur-type prev-type)
      (save-excursion
        (back-to-indentation)
        (setq cur-type (get-text-property (point) 'tag-type)))
      (save-excursion
        (previous-line)
        (back-to-indentation)
        (setq prev-type (get-text-property (point) 'tag-type)))
      (if (or
           (not prev-type cur-type)                   ; both lines sql
           (and (not prev-type) (eq cur-type 'start)) ; sql -> xml
           )
          (sql-indent-line)
        (web-mode-indent-line))))

  (defun mmm-indent-line-web-sql ()
    (interactive)
    (funcall
     (save-excursion
       (back-to-indentation)
       (mmm-update-submode-region)
       (if (and mmm-current-overlay
                (> (overlay-end mmm-current-overlay) (point)))
           'mmm-indent-line-web-sql-submode
         'web-mode-indent-line))))

  (setq mmm-parse-when-idle t)
  (setq mmm-global-classes nil)
  (setq mmm-classes-alist nil)
  (setq mmm-mode-ext-classes-alist nil)
  (setq mmm-indent-line-function 'mmm-indent-line-web-sql)

  (mmm-add-classes
   '((web-sql-select :submode sql-mode
                     :front "<select[^>]*>[ \t]*\n" :back "[ \t]*</select>")
     (web-sql-insert :submode sql-mode
                     :front "<insert[^>]*>[ \t]*\n" :back "[ \t]*</insert>")
     (web-sql-update :submode sql-mode
                     :front "<update[^>]*>[ \t]*\n" :back "[ \t]*</update>")
     (web-sql-delete :submode sql-mode
                     :front "<delete[^>]*>[ \t]*\n" :back "[ \t]*</delete>")
     ))

  (mmm-add-mode-ext-class 'web-mode nil 'web-sql-select)
  (mmm-add-mode-ext-class 'web-mode nil 'web-sql-insert)
  (mmm-add-mode-ext-class 'web-mode nil 'web-sql-update)
  (mmm-add-mode-ext-class 'web-mode nil 'web-sql-delete)
  )

(provide 'init-mmm)
;;; init-mmm.el ends here