;;; init-avy.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package ace-pinyin
  :chords (("jj" . ace-pinyin-jump-char)
           ("jk" . ace-pinyin-jump-char-2))
  :config
  (use-package avy)
  (setq ace-pinyin-use-avy t)
  (ace-pinyin-global-mode)
  :diminish (ace-pinyin-mode))

(provide 'init-avy)
;;; init-avy.el ends here
