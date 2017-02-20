;;; org-time.el --- org time summary
;;
;; Author: ChanningBJ https://github.com/ChanningBJ/OrgTime
;; Maintainer: yangwen0228
;; Created: 周三 二月 15 20:42:27 2017 (+0800)
;; Version: 1.0
;; Package-Requires: (org-mode)
;; URL: https://github.com/yangwen0228/org-time
;; Keywords: org-time
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; Description:
;; This emacs extension is designed to work with emacs org-mode.
;; It can generate a time usage summary from your org-mode file.
;; Giving a table summary of the working time of current month,
;; current week and today (I will not feel guilty when seeing I already
;; have worked more than 40 hours this week ). Also will give a pie
;; chart of the time spend on each tag.
;;
;; Installation:
;; 1. install pychartdir (see http://www.advsofteng.com/download.html for more information)
;; 2. install texttable (see https://pypi.python.org/pypi/texttable for more information)
;; 3. Download and extract the source code
;; 4. Put following code in your .emacs file
;;
;;    (require 'org-time)
;; Usage:
;; M-x: `org-time-summary'
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(defvar org-time-working-dir nil
  "`org-time' working directory. When nil, create a directory named \"orgtime_iamges\"

 under the directory of first org file defined in `org-agenda-files'.")

(defvar org-time-python "python"
  "The python executable path. Prefer to use python3.")

(defconst org-time--cur-dir (file-name-directory load-file-name)
  "The current directory of this file.")

(defun org-time-summary ()
  "Take a screenshot into a unique-named file in the current buffer file

directory and insert a link to this file."
  (interactive)
  (save-buffer)
  (let ((workdir (or org-time-working-dir (file-name-directory )))
        (begin-str "#+BEGIN: clocktable")
        (end-str   "#+END:")
        begin end
        (msg       (shell-command-to-string
                    (concat org-time-python
                            " " (expand-file-name "orgtime.py" org-time--cur-dir)
                            " " buffer-file-name
                            " " org-time-working-dir))))
    (beginning-of-buffer)
    (search-forward begin-str nil t 1)
    (beginning-of-line)
    (setq begin (point))
    (when (search-forward end-str nil t 1)
        (end-of-line))
    (setq end (point))
    (delete-region begin end)

    (insert (concat begin-str " " (format-time-string "%Y-%m-%d %H:%M:%S\n")))
    (insert msg)
    (insert (concat "\n" end-str))
    (when (equal begin end)             ; Run the first time.
        (insert "\n"))
    (org-display-inline-images)))

(provide 'org-time)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-time.el ends here
