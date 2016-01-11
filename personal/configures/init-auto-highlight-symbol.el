;;; init-auto-highlight-symbol.el --- highlight symbol under cursor and edit the symbols
;;; Commentary:
;; Can highlight the symbol under the cursor. And we can specify the region of the symbols.
;; Now we have 3 kinds of regions: 1. current window area;
;;                                 2. the whole buffer;
;;                                 3. function.
;; And we can edit all the matched symbols at the same time.
;; Limitations: don't support user typed symbols or regexp symbols.

;;; Code:

;;   * automatic highlighting current symbol like eclipse IDE.
;;   * cycle through highlighted locations.
;;   * can specify the range to highlight.
;;   * can edit the highlighted symbols at a time.
(use-package auto-highlight-symbol
  :init
  (global-auto-highlight-symbol-mode t)
  (if auto-highlight-symbol-mode-map
      nil
    (setq auto-highlight-symbol-mode-map
          (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-<left>"    ) 'ahs-backward            )
            (define-key map (kbd "M-<right>"   ) 'ahs-forward             )
            (define-key map (kbd "M-S-<left>"  ) 'ahs-backward-definition )
            (define-key map (kbd "M-S-<right>" ) 'ahs-forward-definition  )
            (define-key map (kbd "M--"         ) 'ahs-back-to-start       )
            (define-key map (kbd "C-x C-'"     ) 'ahs-change-range        )
            (define-key map (kbd "C-x C-a"     ) 'ahs-edit-mode           )
            map)))
  :diminish "")

(provide 'init-auto-highlight-symbol)
;;; init-auto-highlight-symbol.el ends here