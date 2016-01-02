;; (require 'w3m-load)
(setq w3m-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8)
(setq w3m-use-cookies t)
(setq w3m-cookie-accept-bad-cookies t)
(setq w3m-home-page
      (if (file-readable-p (expand-file-name "html/home.html" unimacs-tempfiles-dir))
          (concat "file://" (expand-file-name "html/home.html" unimacs-tempfiles-dir))
        "http://www.baidu.com"))

;; set proxy
(defun toggle-env-http-proxy ()
  "set/unset the environment variable http_proxy which w3m uses"
  (interactive)
  (let ((proxy "http://proxynj.huawei.com:8080"))
    (if (string= (getenv "http_proxy") proxy)
        ;; clear the the proxy
        (progn
          (setenv "http_proxy" "")
          (message "env http_proxy is empty now")
          )
      ;; set the proxy
      (progn
        (setenv "http_proxy" proxy)
        (message "env http_proxy is %s now" proxy)
        ))))


(setq w3m-use-toolbar t
      w3m-key-binding 'info)

;; show images in the browser
;; (setq w3m-default-display-inline-images t)
;; (setq w3m-default-toggle-inline-images t)
(setq w3m-view-this-url-new-session-in-background t)
;;显示图标
(setq w3m-show-graphic-icons-in-header-line t)
(setq w3m-show-graphic-icons-in-mode-line t)

(setq w3m-search-default-engine "b")
(eval-after-load "w3m-search" '(progn
                                        ; C-u S g RET <search term> RET
                                 (add-to-list 'w3m-search-engine-alist '("b" "http://www.baidu.com/search?hl=en&q=%s" utf-8))
                                 (add-to-list 'w3m-search-engine-alist '("g" "http://www.google.com.au/search?hl=en&q=%s" utf-8))
                                 (add-to-list 'w3m-search-engine-alist '("wz" "http://zh.wikipedia.org/wiki/Special:Search?search=%s" utf-8))
                                 (add-to-list 'w3m-search-engine-alist '("q" "http://www.google.com.au/search?hl=en&q=%s+site:stackoverflow.com" utf-8))
                                 (add-to-list 'w3m-search-engine-alist '("s" "http://code.ohloh.net/search?s=%s&browser=Default"  utf-8))
                                 (add-to-list 'w3m-search-engine-alist '("bl" "http://blogsearch.google.com.au/blogsearch?q=%s" utf-8))
                                 (add-to-list 'w3m-search-engine-alist '("w" "http://en.wikipedia.org/wiki/Special:Search?search=%s" utf-8))
                                 (add-to-list 'w3m-search-engine-alist '("d" "http://dictionary.reference.com/search?q=%s" utf-8))
                                 (add-to-list 'w3m-search-engine-alist '("j" "http://www.google.com.au/search?ie=UTF-8&oe=UTF-8&sourceid=navclient&btnI=1&q=%s+site:developer.mozilla.org" utf-8))
                                 ))

(setq w3m-command-arguments       '("-F" "-cookie")
      w3m-mailto-url-function     'compose-mail
      browse-url-browser-function 'w3m
      mm-text-html-renderer       'w3m)

                                        ;bind this function to ‘a’, which is the normal w3m bookmark binding:
(eval-after-load "w3m" '(progn
                          (w3m-lnum-mode 1)
                          ))

                                        ; external browser
(setq browse-url-generic-program
      (cond
       (*is-a-mac* "open")
       (*linux* (executable-find "firefox"))
       ))
(setq browse-url-browser-function 'browse-url-generic)

;; use external browser to search programming stuff
(defun w3mext-hacker-search ()
  "search word under cursor in google code search and stackoverflow.com"
  (interactive)
  (require 'w3m)
  (let ((keyword (w3m-url-encode-string (thing-at-point 'symbol))))
    ;; google
    (browse-url-generic (concat "http://www.google.com.au/search?hl=en&q=%22"
                                keyword
                                "%22"
                                (if buffer-file-name
                                    (concat "+filetype%3A" (file-name-extension buffer-file-name))
                                  "")  ))
    (browse-url-generic (concat "http://www.google.com.au/search?hl=en&q="
                                keyword
                                "+site:stackoverflow.com" ))
    ;; koders.com
    (browse-url-generic (concat "http://code.ohloh.net/search?s=\""
                                keyword
                                "\"&browser=Default&mp=1&ml=1&me=1&md=1&filterChecked=true" ))
    ))

(defun w3mext-open-link-or-image-or-url ()
  "Opens the current link or image or current page's uri or any url-like text under cursor in firefox."
  (interactive)
  (let (url)
    (if (or (string= major-mode "w3m-mode") (string= major-mode "gnus-article-mode"))
        (setq url (or (w3m-anchor) (w3m-image) w3m-current-url)))
    (browse-url-generic (if url url (car (browse-url-interactive-arg "URL: "))))
    ))
(global-set-key (kbd "C-c b") 'w3mext-open-link-or-image-or-url)

(defun w3mext-search-js-api-mdn ()
  "search current symbol under cursor in Mozilla Developer Network (MDN)"
  (interactive)
  (let ((keyword (thing-at-point 'symbol)))
    (w3m-search "j" keyword)
    ))

(add-hook 'prog-mode-hook '( lambda () (local-set-key (kbd "C-c ; h") 'w3mext-hacker-search)))

;; Find in blog.
;; @see: http://blog.csdn.net/superwen_go/article/details/8241601
(add-hook 'w3m-fontify-after-hook 'remove-w3m-output-garbages)
(defun remove-w3m-output-garbages ()
  "去掉w3m输出的垃圾."
  (interactive)
  (let ((buffer-read-only))
    (setf (point) (point-min))
    (while (re-search-forward "[\200-\240]" nil t)
      (replace-match " "))
    (set-buffer-multibyte t))
  (set-buffer-modified-p nil))

  (provide 'init-emacs-w3m)
