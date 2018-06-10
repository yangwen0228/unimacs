;;; init-cmake-mode.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(provide 'init-cmake-mode)
;;; init-cmake-mode.el ends here
