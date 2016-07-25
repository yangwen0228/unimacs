;;; init-hideshow.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package hideshow
  :init (add-hook 'prog-mode-hook 'hs-minor-mode)
  :config
  (use-package hideshowvis
    :init (add-hook 'prog-mode-hook 'hideshowvis-enable)
    :config
    (hideshowvis-symbols)
    (defun display-code-line-counts (ov)
        (when (eq 'code (overlay-get ov 'hs))
          (let* ((marker-string "*fringe-dummy*")
                 (marker-length (length marker-string)))
            (put-text-property 0 marker-length 'display (list 'left-fringe 'hs-marker 'hs-fringe-face) marker-string)
            (overlay-put ov 'before-string marker-string)))))
  (setq hs-allow-nesting t)
  (define-key hs-minor-mode-map (kbd "C-z m") 'hs-hide-all)
  (define-key hs-minor-mode-map (kbd "C-z r") 'hs-show-all)
  (define-key hs-minor-mode-map (kbd "C-z c") 'hs-hide-block)
  (define-key hs-minor-mode-map (kbd "C-z o") 'hs-show-block)
  (define-key hs-minor-mode-map (kbd "C-z C") 'hs-)
  (define-key hs-minor-mode-map (kbd "C-z O") 'hs-)
  (define-key hs-minor-mode-map (kbd "C-z l") 'hs-hide-level)
  (define-key hs-minor-mode-map (kbd "C-z C-z") 'hs-toggle-hiding)

  :diminish (hs-minor-mode))

(provide 'init-hideshow)
;;; init-hideshow.el ends here
