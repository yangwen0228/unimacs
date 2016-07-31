;;; package --- Summary :  init-auctex.el
;;; Commentary:
;; AucTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)

;; (add-hook 'LaTeX-mode-hook (lambda ()
;;                              (push
;;                               '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
;;                                 :help "Run latexmk on file")
;;                               TeX-command-list)))
;; (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))
(add-hook 'LaTeX-mode-hook (lambda ()
                             (add-to-list 'TeX-command-list
                              '("latexmk" "latexmk -e '$pdflatex=q/xelatex -synctex=1 -interaction=nonstopmode/' -pvc -pdf %s" TeX-run-TeX nil t
                                :help "Run latexmk on file"))))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

(add-hook 'LaTeX-mode-hook (lambda ()
                             (add-to-list 'TeX-command-list
                              '("XeLaTex" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t
                                :help "Run XeLaTex on file"))
                             (setq TeX-save-query  nil )
                             (setq TeX-show-compilation t) 
                             ))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "XeLaTex")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

(provide 'init-auctex)
;;; init-auctex.el ends here