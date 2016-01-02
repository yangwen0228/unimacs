;;; personal-init.el --- Summary:
;; You customize your personel lisp codes here.

;;; Commentary:

;;; code:
;; -*- coding: utf-8 -*-
;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
(defun require-and-configure (configure)
  (condition-case err
      (require configure nil 'noerror)
    (message (error "when load %s : %s" package err))))

(setq unimacs-init-packages
      '(
        ;;-------------------------------------------------------------------
        ;; basic functions && frame
        ;;-------------------------------------------------------------------
        init-funcs
        init-emacs
        init-misc
        init-exec-path
        init-recentf
        init-dired+
        init-bind-key
        init-byte-compile
        ;;-------------------------------------------------------------------
        ;; interface
        ;;-------------------------------------------------------------------
        init-linum-mode
        init-flx
        init-projectile
        init-window-numbering
        ;; init-powerline
        ;; init-smart-mode-line
        
        init-auto-highlight-symbol
        init-highlight-chars
        init-rainbow-delimiters

        init-ido
        init-ido-vertical

        init-helm
        init-helm-projectile
        init-helm-swoop
        init-helm-descbinds
        init-helm-flycheck
        init-helm-ag
        init-helm-gtags
        ;;-------------------------------------------------------------------
        ;; editing
        ;;-------------------------------------------------------------------
        ;; init-mmm
        init-avy
        ;;init-ace-jump-mode

        ;;init-evil
        ;;init-evil-matchit
        ;;init-evil-mc 'evil-mc
        ;;init-surround

        init-multiple-cursors
        init-undo-tree
        init-smartparens
        init-align

        ;; evil replacement
        init-origami ;  foldin
        init-origami-tcl
        init-hideshowvis

        init-expand-region ; expand regio
        init-comment-dwim-2 ; evil-nerd-commentar

        ;;-------------------------------------------------------------------
        ;; auto company && auto check
        ;;-------------------------------------------------------------------
        init-company
        init-yasnippet
        init-auto-yasnippet
        init-flycheck
        ;; init-flyspell

        ;;-------------------------------------------------------------------
        ;; programming languages
        ;;-------------------------------------------------------------------
        init-emacs-w3m
        init-elisp
        init-python-mode
        init-tcl-hm-mode
        init-header2
        init-markdown
        ;; init-haskell
        ;; init-rails
        ;; init-ruby-mode
        ;; init-csharp-mode
        ;; init-clojure-cider
        ;; init-cmake-mode
        ;; init-lua-mode
        ;; init-javascript
        ;; init-html
        ;; init-erlang

        ;; init-cc-mode
        ;; init-irony
        ;; init-irony-eldoc
        ;; init-cpputils-cmake
        ;; init-compile
        ;;-------------------------------------------------------------------
        ;; writting
        ;;-------------------------------------------------------------------
        init-org
        init-org-mime
        init-cnblogs
        ;;init-auctex
        ;; init-darcs
        ;; init-doxygen
        ;; init-frame-hooks
        ;; init-gist
        ;; init-git
        ;; init-gnus
        ;; init-gud
        ;; init-gui-frames
        ;; init-spelling
        ;; init-keyfreq
        ;; init-ledger
        ;; init-osx-keys
        ;; init-popwin
        ;; init-proxies
        ;; init-sql
        ;; init-sr-speedbar
        ;; init-vc
        ;;-------------------------------------------------------------------
        ;; modules not offen used
        ;;-------------------------------------------------------------------
        ;; init-bbdb
        ;; init-css
        ;; init-csv
        ;; init-elnode
        ;; init-emms
        ;; init-haml
        ;; init-moz
        ;; init-sessions
        ;; init-sh
        ;; init-slime
        ;; init-term-mode
        ;; init-textile
        ;; init-themes
        ;; init-uniquify
        ;; init-web-mode
        ;; init-which-func
        ;; init-windows
        ;; init-xterm
        ;; init-yari
        ;; init-zencoding-mode
        ;; init-modeline
        ;; init-nyan
        ;; init-semantic
        ;; init-crontab
        ;; init-workgroups2
        ;; init-sunrise-commander
        init-fonts
        init-maxframe
        init-diminish
        ))

(mapc 'require-and-configure unimacs-init-packages)
;;; personal-init.el ends here
