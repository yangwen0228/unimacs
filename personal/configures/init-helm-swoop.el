;;; init-helm-swoop.el --- Summary
;;; Commentary:
;; comments

;;; Code:
;; helm from https://github.com/emacs-helm/helm
(use-package helm-swoop
  :bind (("M-i"     . helm-swoop)
         ("M-I"     . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all)
         :map helm-swoop-edit-map
         ("C-c C-c" . helm-swoop--edit-complete)
         ("C-c C-k" . helm-swoop--edit-cancel)
         ("C-c C-d" . helm-swoop--edit-delete-all-lines))
  :config
  ;; When doing isearch, hand the word over to helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  ;; "M-i" again in helm-multi-swoop mode will lead to an error:
  (defun NoOp () (interactive))
  (define-key helm-multi-swoop-map (kbd "M-i") 'NoOp)
  ;; Move up and down like isearch
  (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
  (define-key helm-multi-swoop-map (kbd "c-r") 'helm-previous-line)
  (define-key helm-multi-swoop-map (kbd "c-s") 'helm-next-line)

  ;; save buffer when helm-multi-swoop-edit complete
  (setq helm-multi-swoop-edit-save t)

  ;; if this value is t, split window inside the current window
  (setq helm-swoop-split-with-multiple-windows nil)

  ;; split direcion. 'split-window-vertically or 'split-window-horizontally
  (setq helm-swoop-split-direction 'split-window-horizontally)

  ;; if nil, you can slightly boost invoke speed in exchange for text color
  (setq helm-swoop-speed-or-color nil)

  ;; go to the opposite side of line from the end or beginning of line
  (setq helm-swoop-move-to-line-cycle nil)

  ;; optional face for line numbers
  ;; face name is `helm-swoop-line-number-face`
  (setq helm-swoop-use-line-number-face t)
  )


(provide 'init-helm-swoop)
;;; init-helm-swoop.el ends here
