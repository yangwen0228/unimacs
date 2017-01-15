;;; init-avy.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package ace-pinyin
  :bind (("M-g j"   . ace-pinyin-jump-char)
         ("M-g M-g" . ace-pinyin-jump-char-2)
         ("M-g s"   . avy-goto-word-or-subword-1))
  :config
  (use-package avy)
  (setq ace-pinyin-use-avy t)
  (ace-pinyin-global-mode)
  :diminish (ace-pinyin-mode))

(provide 'init-avy)
;;; init-avy.el ends here