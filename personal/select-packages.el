;;; personal-init.el --- Summary:
;; You customize your personel lisp codes here.

;;; Commentary:
;; Don't use linum-mode, it's very slow when scroll in big files.
;; Don't use showhidevis, it has problem to open big files.

;;; code:
;; -*- coding: utf-8 -*-
;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
(defun require-and-configure (configure)
  (condition-case err
      (require configure nil 'noerror)
    (error (message "Error: when load %s : %s" configure err))))

(setq unimacs-init-packages
      '(
        ;;-------------------------------------------------------------------
        ;; basic functions && frame
        ;;-------------------------------------------------------------------
        init-emacs
        init-exec-path
        init-dired
        init-maxframe
        ;;-------------------------------------------------------------------
        ;; interface
        ;;-------------------------------------------------------------------
        init-window-numbering
        init-switch-window
        init-rainbow-delimiters
        init-ido
        init-helm
        init-helm-projectile
        init-helm-swoop
        init-helm-descbinds
        init-helm-flycheck
        init-helm-flyspell
        init-helm-ag
        init-helm-gtags
        ;;-------------------------------------------------------------------
        ;; editing
        ;;-------------------------------------------------------------------
        init-keyfreq
        init-avy
        init-pinyin-search
        init-string-edit
        init-iedit
        init-jumplist
        init-multiple-cursors
        init-undo-tree
        init-smartparens
        init-align
        ;; evil replacement
        init-hideshow ;  folding
        ;; init-origami ;  folding
        init-comment-dwim-2 ; evil-nerd-commentar
        ;;-------------------------------------------------------------------
        ;; auto company && auto check
        ;;-------------------------------------------------------------------
        init-company
        init-yasnippet
        init-auto-yasnippet
        init-mvc
        ;;-------------------------------------------------------------------
        ;; programming languages
        ;;-------------------------------------------------------------------
        ;; init-header2
        init-emacs-w3m
        init-python-mode
        init-tcl-hm-mode
        init-markdown
        init-web-mode
        init-emmet
        init-javascript
        init-css
        ;; init-html
        ;; init-haskell
        ;; init-rails
        ;; init-ruby-mode
        ;; init-csharp-mode
        ;; init-clojure-cider
        ;; init-cmake-mode
        ;; init-lua-mode
        ;; init-erlang

        ;; init-cc-mode
        ;; init-irony
        ;; init-irony-eldoc
        ;; init-cpputils-cmake
        ;; init-compile

        ;; init-sql
        ;; init-csv
        ;; init-haml
        ;; init-slime
        ;;-------------------------------------------------------------------
        ;; writting
        ;;-------------------------------------------------------------------
        init-org
        ;; init-org-mime
        ;; init-auctex
        ;; init-darcs
        ;;-------------------------------------------------------------------
        ;; modules not offen used
        ;;-------------------------------------------------------------------
        ;; init-doxygen
        init-vc
        ;; init-gist
        ;; init-git
        ;; init-gnus
        ;; init-gud
        ;; init-osx-keys
        ;; init-proxies
        ;; init-sr-speedbar
        ;; init-bbdb
        ;; init-elnode
        ;; init-emms
        ;; init-moz
        ;; init-sessions
        ;; init-term-mode
        ;; init-textile
        ;; init-uniquify
        ;; init-xterm
        ;; init-semantic
        ;; init-crontab
        ;; init-sunrise-commander
        ;; init-workgroups2
        ))

(mapc 'require-and-configure unimacs-init-packages)
;;; personal-init.el ends here
