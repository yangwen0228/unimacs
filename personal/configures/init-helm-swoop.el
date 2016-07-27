;;; init-helm-swoop.el --- Summary
;;; Commentary:
;; comments

;;; Code:
;; helm from https://github.com/emacs-helm/helm
(use-package helm-swoop
  :bind (("M-i"     . helm-swoop)
         ("M-I"     . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all))
  :config
  ;; When doing isearch, hand the word over to helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  ;; From helm-swoop to helm-multi-swoop-all
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  ;; When doing evil-search, hand the word over to helm-swoop
  ;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

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

  (defface helm-swoop-target-line-face
    '((t (:background "#e3e300" :foreground "#222222")))
    "Face for helm-swoop target line"
    :group 'helm-swoop)
  (defface helm-swoop-target-line-block-face
    '((t (:background "#cccc00" :foreground "#222222")))
    "Face for target line"
    :group 'helm-swoop)
  (defface helm-swoop-target-word-face
    '((t (:background "#7700ff" :foreground "#ffffff")))
    "Face for target word"
    :group 'helm-swoop)
  (defface helm-swoop-line-number-face
    '((t (:foreground "#999999")))
    "Face for line numbers"
    :group 'helm-swoop)
  )


(provide 'init-helm-swoop)
;;; init-helm-swoop.el ends here
