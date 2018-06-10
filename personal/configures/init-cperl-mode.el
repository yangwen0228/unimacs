;;; init-cperl-mode.el --- Summary
;;; Commentary:
;; comments

;;; Code:
(use-package cperl-mode :ensure nil
  :mode ("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode)
  :interpreter (("perl" . cperl-mode)
                ("perl5" . cperl-mode)
                ("miniperl" . cperl-mode)))

(provide 'init-cperl-mode)
;;; init-cperl-mode.el ends here