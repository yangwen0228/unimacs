;;; personal-init.el --- Summary:
;; You customize your personel lisp codes here.

;;; Commentary:

;;; code:
;; -*- coding: utf-8 -*-
;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
(defun require-and-configure (configure &optional package)
  (condition-case err
      (progn
        (if package (prelude-require-package package))
        (require configure nil 'noerror))
    (error (error "when load %s : %s" package err))
    ))
;;----------------------------------------------------------------------------
;; basic functions && frame
;;----------------------------------------------------------------------------
(require-and-configure 'init-funcs) ;; must come first
(require-and-configure 'init-emacs)
(require-and-configure 'init-misc)
(require-and-configure 'init-exec-path) ;; Set up $PATH
(require-and-configure 'init-recentf)

(require-and-configure 'init-dired+)
(require-and-configure 'init-bind-key)
(require-and-configure 'init-byte-compile)
;;----------------------------------------------------------------------------
;; interface
;;----------------------------------------------------------------------------
(require-and-configure 'init-linum-mode)
(require-and-configure 'init-flx)
(require-and-configure 'init-projectile)
(require-and-configure 'init-window-numbering)

;; (require-and-configure 'init-powerline)
;; (require-and-configure 'init-smart-mode-line)
(require-and-configure 'init-auto-highlight-symbol)
(require-and-configure 'init-highlight-chars)
(require-and-configure 'init-rainbow-delimiters)

(require-and-configure 'init-ido)
(require-and-configure 'init-ido-vertical)

(require-and-configure 'init-helm)
(require-and-configure 'init-helm-projectile)
(require-and-configure 'init-helm-swoop)
(require-and-configure 'init-helm-descbinds)
(require-and-configure 'init-helm-flycheck)
(require-and-configure 'init-helm-ag)
(require-and-configure 'init-helm-gtags)
;;----------------------------------------------------------------------------
;; editing
;;----------------------------------------------------------------------------
(require-and-configure 'init-avy)
;;(require-and-configure 'init-ace-jump-mode)

;;(require-and-configure 'init-evil)
;;(require-and-configure 'init-evil-matchit)
;;(require-and-configure 'init-evil-mc 'evil-mc)
;;(require-and-configure 'init-surround)
;;(require-and-configure 'init-paredit)

(require-and-configure 'init-undo-tree)
(require-and-configure 'init-smartparens)
;;----------------------------------------------------------------------------
;; evil replacement
;;----------------------------------------------------------------------------
(require-and-configure 'init-multiple-cursors)

(require-and-configure 'init-origami) ;  folding
(require-and-configure 'init-origami-tcl)
(require-and-configure 'init-hideshowvis)

(require-and-configure 'init-expand-region) ; expand region
(require-and-configure 'init-comment-dwim-2) ; evil-nerd-commentary

;;----------------------------------------------------------------------------
;; auto company && auto check
;;----------------------------------------------------------------------------
(require-and-configure 'init-company)
(require-and-configure 'init-yasnippet)
(require-and-configure 'init-auto-yasnippet)
(require-and-configure 'init-flycheck)

;;----------------------------------------------------------------------------
;; programming languages
;;----------------------------------------------------------------------------
(require-and-configure 'init-emacs-w3m)
(require-and-configure 'init-elisp)
(require-and-configure 'init-python-mode)
(require-and-configure 'init-tcl-hm-mode)
(require-and-configure 'init-header2)
(require-and-configure 'init-markdown)
;; (require-and-configure 'init-haskell)
;; (require-and-configure 'init-rails)
;; (require-and-configure 'init-ruby-mode)
;; (require-and-configure 'init-csharp-mode)
;; (require-and-configure 'init-clojure-cider)
;; (require-and-configure 'init-cmake-mode)
;; (require-and-configure 'init-lisp)
;; (require-and-configure 'init-lua-mode)
;; (require-and-configure 'init-javascript)
;; (require-and-configure 'init-html)
;; (require-and-configure 'init-erlang)

;; (require-and-configure 'init-cc-mode)
;; (require-and-configure 'init-irony)
;; (require-and-configure 'init-irony-eldoc)
;; (require-and-configure 'init-cpputils-cmake)
;; (require-and-configure 'init-compile)
;;----------------------------------------------------------------------------
;; writting
;;----------------------------------------------------------------------------
(require-and-configure 'init-org)
(require-and-configure 'init-org-mime)
(require-and-configure 'init-cnblogs)

;;(require-and-configure 'init-auctex)

;; not finished
;; (require-and-configure 'init-darcs)
;; (require-and-configure 'init-dash)
;; (require-and-configure 'init-doxygen)

;; (require-and-configure 'init-emacspeak)

;; (require-and-configure 'init-frame-hooks)
;; (require-and-configure 'init-gist)
;; (require-and-configure 'init-git)
;; (require-and-configure 'init-gnus)
;; (require-and-configure 'init-gud)
;; (require-and-configure 'init-gui-frames)
;; (require-and-configure 'init-hippie-expand)
;; (require-and-configure 'init-spelling)
;; (require-and-configure 'init-keyfreq)
;; (require-and-configure 'init-ledger)


;; (require-and-configure 'init-mmm)
;; (require-and-configure 'init-osx-keys)
;; (require-and-configure 'init-popwin)
;; (require-and-configure 'init-proxies)


;; (require-and-configure 'init-sql)
;; (require-and-configure 'init-sr-speedbar)
;; (require-and-configure 'init-vc)

;; modules not offen used
;; (require-and-configure 'init-bbdb)
;; (require-and-configure 'init-css)
;; (require-and-configure 'init-csv)
;; (require-and-configure 'init-elnode)
;; (require-and-configure 'init-emms)
;; (require-and-configure 'init-haml)
;; (require-and-configure 'init-moz)



;; (require-and-configure 'init-sessions)
;; (require-and-configure 'init-sh)
;; (require-and-configure 'init-slime)
;; (require-and-configure 'init-term-mode)
;; (require-and-configure 'init-textile)
;; (require-and-configure 'init-themes)
;; (require-and-configure 'init-uniquify)
;; (require-and-configure 'init-web-mode)
;; (require-and-configure 'init-which-func)
;; (require-and-configure 'init-windows)
;; (require-and-configure 'init-xterm)
;; (require-and-configure 'init-yari)
;; (require-and-configure 'init-zencoding-mode)
;; (require-and-configure 'init-modeline)
;; (require-and-configure 'init-nyan)


;; beated packages ;;;;;;;;;;;;;;;;;;;;;;;;
;; (require-and-configure 'init-semantic)

;; (require-and-configure 'init-crontab)
;; (require-and-configure 'init-workgroups2)
;; (require-and-configure 'init-sunrise-commander)
(require-and-configure 'init-fonts)
(require-and-configure 'init-maxframe)

;;; personal-init.el ends here
