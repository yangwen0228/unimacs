;;; init-origami.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package origami
  :bind-keymap ("C-z" . origami-mode-map)
  :config
  (global-origami-mode t)
  (setq origami-show-fold-header t)
  ;; (regexp "proc\\s-*\\(\\s_\\|\\w\\|[:?!]\\)*\\([ \\t]*{.*?}\\)?")
  ;; (defun origami-rebuild-tree? (buffer)
  ;;   "Debug use only"
  ;;   t)
  ;; (defun origami-get-parser (buffer)
  ;;   "Debug use only"
  ;;   (lexical-let* ((cached-tree (origami-get-cached-tree buffer))
  ;;                  (create (lambda (beg end offset children)
  ;;                            (let ((previous-fold (-last-item (origami-fold-find-path-with-range cached-tree beg end))))
  ;;                              (origami-fold-node beg end offset
  ;;                                                 (if previous-fold (origami-fold-open? previous-fold) t)
  ;;                                                 children
  ;;                                                 (or (-> (origami-fold-find-path-with-range
  ;;                                                          (origami-get-cached-tree buffer) beg end)
  ;;                                                         -last-item
  ;;                                                         origami-fold-data)
  ;;                                                     (origami-create-overlay beg end offset buffer)))))))
  ;;     (-when-let (parser-gen (or (cdr (assoc (if (local-variable-p 'origami-fold-style)
  ;;                                                (buffer-local-value 'origami-fold-style buffer)
  ;;                                              (buffer-local-value 'major-mode buffer))
  ;;                                            origami-parser-alist))
  ;;                                'origami-indent-parser))
  ;;       (funcall parser-gen create))))
  ;; (defun origami-tcl-parser (create)
  ;;   (lexical-let ((create create))
  ;;     (lambda (content)
  ;;       (let ((positions (->> (origami-get-positions content "[{}]")
  ;;                             (remove-if (lambda (position)
  ;;                                          (let ((face (get-text-property 0 'face (car position))))
  ;;                                            (-any? (lambda (f)
  ;;                                                     (memq f '(font-lock-doc-face
  ;;                                                               font-lock-comment-face
  ;;                                                               font-lock-string-face)))
  ;;                                                   (if (listp face) face (list face))))))
  ;;                             )
  ;;                        ))
  ;;         (setq positions (-flatten (remove-if (lambda (pair)
  ;;                                      (print pair)
  ;;                                      (let ((beg (cdr (nth 0 pair)))
  ;;                                            (end (cdr (nth 1 pair))))
  ;;                                        (print beg)
  ;;                                        (print end)
  ;;                                        )
  ;;                                      nil)
  ;;                                    (-partition-all 2 positions))))
  ;;         (print positions)
  ;;         (origami-build-pair-tree create "{" "}" positions)))
  ;;     )
  ;;   )
  (defun origami-tcl-parser (create)
    (lexical-let ((create create))
      (lambda (content)
        (let ((positions (->> (origami-get-positions content "{[ \t]*$\\|}[ \t]*$")
                              (remove-if (lambda (position)
                                           (let ((face (get-text-property 0 'face (car position))))
                                             (-any? (lambda (f)
                                                      (memq f '(font-lock-doc-face
                                                                font-lock-comment-face
                                                                font-lock-string-face)))
                                                    (if (listp face) face (list face))))))
                              )
                         ))
          (print positions)
          (origami-build-pair-tree create "{" "}" positions)))
      )
    )

  (add-to-list 'origami-parser-alist '(tcl-mode . origami-tcl-parser))
  (define-key origami-mode-map (kbd "C-z m") 'origami-close-all-nodes)
  (define-key origami-mode-map (kbd "C-z c") 'origami-close-node)
  (define-key origami-mode-map (kbd "C-z C") 'origami-close-node-recursively)
  (define-key origami-mode-map (kbd "C-z r") 'origami-open-all-nodes)
  (define-key origami-mode-map (kbd "C-z o") 'origami-open-node)
  (define-key origami-mode-map (kbd "C-z O") 'origami-open-node-recursively)
  )

(provide 'init-origami)
;;; init-origami.el ends here