;; You should download a keyhh.exe file. http://api.256file.com/download/66010_keyhh.exe
;; Copy some codes from: https://github.com/fanhongtao/_emacs.d/blob/master/lisp/keyword-help.el
(defgroup chm-help nil
  "Open and view chm document help pages through keywords!"
  :group 'help)

(defcustom chm-help-lookup-alist
  (list
   '("tcl-mode" . "c:/Tcl8.6/doc/ActiveTclHelp8.6.chm"))
  "The prog mode and file path alist"
  :type '(repeat (cons (string :tag "Program mode")
                       (string :tag "Chm file path")))
  :group 'chm-help)

;; CHM (HtmlHelp)
;;note: KeyHH.exe needs to be in $PATH.
;;KeyHH -MyHelp -#klink "ActiveX Control Wizard" htmlhelp.chm
(defun chm-help--lookup-chm (chm-file keyword)
  "Lookup a KEYWORD in a CHM-FILE and display it."
  (start-process "keyhh" nil
                 "keyhh.exe"
                 (concat "-" (symbol-name major-mode))
                 "-#alink" (format "'%s'" keyword)
                 chm-file)
  (set-process-query-on-exit-flag (get-process "keyhh") nil))

;; MS Help 2 (MSDN)
;; http://www.emacswiki.org/emacs/MsdnHelp
;; You need `h2viewer' utility from
;;      http://www.helpware.net/mshelp2/h2viewer.htm
;; invoke it as this:
;;      h2viewer /helpcol MS.PSDK.1033 /keyword K$CreateWindowEx
(defun chm-help--lookup-hh2 (helpcol keyword)
  "Open a window showing the MSDN documentation for the word under the point"
  (if (string= "w32" window-system)
      (start-process "h2viewer" nil
                     "h2viewer.exe"
                     "/appID" "MSDN-Viewer"
                     "/helpcol" helpcol
                     ;;                      "/filtername" "Visual C++"
                     "/keyword" (concat "K$" (current-word)))))

;;;###autoload
(defun chm-help-lookup ()
  (interactive)
  (let ((keyword (if (and transient-mark-mode mark-active)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (thing-at-point 'symbol)))
        (chm-file (cdr (assoc (symbol-name major-mode) chm-help-lookup-alist))))
    (setq keyword (read-string "Keyword: " keyword))
    (if (or (not chm-file) (not (f-exists-p chm-file)))
        (setq chm-file (read-file-name "Chm file: ")))

    (when (and chm-file (f-exists-p chm-file))
      (add-to-list 'chm-help-lookup-alist
                   (cons (symbol-name major-mode) chm-file))
      (chm-help--lookup-chm chm-file keyword))))

(global-set-key (kbd "C-c h") 'chm-help-lookup)

(provide 'chm-help)