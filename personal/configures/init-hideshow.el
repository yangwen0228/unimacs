;;; init-hideshow.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package hideshow
  :init (add-hook 'prog-mode-hook 'hs-minor-mode)
  :config
  (use-package hideshowvis
    :disabled
    :init (add-hook 'prog-mode-hook 'hideshowvis-enable)
    :config
    (hideshowvis-symbols)
    (defun display-code-line-counts (ov)
      (when (eq 'code (overlay-get ov 'hs))
        (let* ((marker-string "*fringe-dummy*")
               (marker-length (length marker-string)))
          (put-text-property 0 marker-length 'display (list 'left-fringe 'hs-marker 'hs-fringe-face) marker-string)
          (overlay-put ov 'before-string marker-string)))))

  (defun my-hs-hide-block (&optional end)
    "Bugfix: cursor not at the END, can't fold."
    (interactive "P")
    (save-excursion
      (goto-char (line-end-position))
      (hs-hide-block end)))
  (defun my-hs-show-block (&optional end)
    "Bugfix: cursor not at the END, the cursor go to end."
    (interactive "P")
    (save-excursion
      (hs-show-block end)))
  (defun my-hs-toggle-hiding (&optional end)
    "Bugfix: not at the END, the cursor go to end."
    (interactive "P")
    (if (hs-already-hidden-p)
        (my-hs-show-block end)
      (my-hs-hide-block end)))
  (defun my-hs-show-block-recursively (&optional end)
    "Bugfix: not at the END, the cursor go to end."
    (interactive "P")
    )
  (defun hs-hide-level-recursive (arg minp maxp)
    "Recursively hide blocks ARG levels below point in region (MINP MAXP)."
    (when (hs-find-block-beginning)
      (setq minp (1+ (point)))
      (funcall hs-forward-sexp-func 1)
      (setq maxp (1- (point))))
    (unless hs-allow-nesting
      (hs-discard-overlays minp maxp))
    (goto-char minp)
    (let ((re (concat "\\("
                      hs-block-start-regexp
                      "\\)"
                      (if hs-hide-comments-when-hiding-all
                          (concat "\\|\\("
                                  hs-c-start-regexp
                                  "\\)")
                        ""))))
      (while (progn
               (unless hs-hide-comments-when-hiding-all
                 (forward-comment (buffer-size)))
               (and (< (point) maxp)
                    (re-search-forward re maxp t)))
        (if (> arg 1)
            (hs-hide-level-recursive (1- arg) minp maxp)
          (if (match-beginning 1)
              ;; We have found a block beginning.
              (progn
                (goto-char (match-beginning 1))
                (unless (if hs-hide-all-non-comment-function
                            (funcall hs-hide-all-non-comment-function)
                          (hs-hide-block-at-point t))
                  ;; Go to end of matched data to prevent from getting stuck
                  ;; with an endless loop.
                  (goto-char (match-end 0))))
            ;; found a comment, probably
            (let ((c-reg (hs-inside-comment-p)))
              (when (and c-reg (car c-reg))
                (if (> (count-lines (car c-reg) (nth 1 c-reg)) 1)
                    (hs-hide-block-at-point t c-reg)
                  (goto-char (nth 1 c-reg)))))))))
    (goto-char maxp))
  (setq hs-allow-nesting t)
  (define-key hs-minor-mode-map (kbd "C-z C-n") 'hs-hide-all)
  (define-key hs-minor-mode-map (kbd "C-z <C-m>") 'hs-show-all)
  (define-key hs-minor-mode-map (kbd "C-z C-c") 'my-hs-hide-block)
  (define-key hs-minor-mode-map (kbd "C-z C-o") 'my-hs-show-block)
  (define-key hs-minor-mode-map (kbd "C-z O") 'my-hs-show-block-recursively)
  (define-key hs-minor-mode-map (kbd "C-z C-l") 'hs-hide-level)
  (define-key hs-minor-mode-map (kbd "C-z C-z") 'my-hs-toggle-hiding)

  :diminish (hs-minor-mode))

(provide 'init-hideshow)
;;; init-hideshow.el ends here
