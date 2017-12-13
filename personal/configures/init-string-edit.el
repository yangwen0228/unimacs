;;; init-string-edit.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package string-edit
  :bind ("C-c C-e" . string-edit-at-point)
  :config
  (defun se/find-original ()
    (if (derived-mode-p 'js2-mode 'js-mode 'java-mode)
        (se/js-strings-at-point)
      (se/string-at-point)))
  ;; override
  ;; make it can choose major-mode for string
  (defun string-edit-at-point ()
    "Pop up a buffer to edit the string at point.
This saves you from needing to manually escape characters."
    (interactive)
    (let ((mode (helm-comp-read "Mode: "
                                (mapcar '(lambda (mode) (concat mode "-mode"))
                                        (scratch-list-modes))
                                :exec-when-only-one t
                                :default (symbol-name major-mode))))
      (when (se/point-inside-string-p)
        (let* ((p (point))
               (original-buffer (current-buffer))
               (original (se/find-original)))
          (select-window (split-window-vertically -4))
          (switch-to-buffer (generate-new-buffer "*string-edit*"))
          (insert (se/aget :raw original))
          (goto-char (- p (se/aget :beg original) -1))
          (funcall (se/aget :cleanup original))
          (enlarge-window (1- (line-number-at-pos (point-max))))
          ;; (se/guess-at-major-mode)
          (funcall (intern mode))
          (run-hooks 'string-edit-at-point-hook)
          (string-edit-mode 1)
          (set (make-local-variable 'se/original) original)
          (set (make-local-variable 'se/original-buffer) original-buffer)
          (font-lock-fontify-buffer))))))

(provide 'init-string-edit)
;;; init-string-edit.el ends here