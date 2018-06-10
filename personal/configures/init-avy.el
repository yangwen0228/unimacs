;;; init-avy.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package ace-pinyin
  :chords (("jj" . ace-pinyin-jump-char-2)
           ("jk" . ace-pinyin-jump-char))
  :init (setq ace-pinyin-use-avy t)
  :config
  (use-package avy)
  (ace-pinyin-global-mode 1)
  :diminish (ace-pinyin-mode))

(provide 'init-avy)
;;; init-avy.el ends here
