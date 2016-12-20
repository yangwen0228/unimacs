;;; init-pinyin-search.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package pinyin-search
  :bind ("C-s" . isearch-forward-regexp)
  :bind ("C-r" . isearch-backward-regexp)
  :bind ("C-S-s" . pinyin-search)
  )

(provide 'init-pinyin-search)
;;; init-pinyin-search.el ends here