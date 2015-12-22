;;; init-clang.el --- Summary:
;; Configure all the setting related to clang.
;;
;; Including clang include dir, clang flags under difference operate systems.

;;; Commentary:
;; Packages can share the settings: auto-complete-mode company-mode irony flycheck.

;;; Code:
(setq clang-include-dir-str
    (cond
     (*win32* "
          C:\\Program Files (x86)\\Microsoft Visual Studio 12.0\\VC\\include
          ")
     (*is-a-mac* "
          /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/c++/v1
          /usr/local/include
          /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/5.1/include
          /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include
          /usr/include
          /System/Library/Frameworks
          /Library/Frameworks
          ")
     (*mingw32* "
          c:/DXSDK/include
          c:/MinGW/include
          c:/MinGW/lib/gcc/mingw32/4.8.1/inlucde
          c:/MinGW/lib/gcc/mingw32/4.8.1/inlucde/c++
          c:/MinGW/lib/gcc/mingw32/4.8.1/inlucde/c++/bits
          c:/MinGW/lib/gcc/mingw32/4.8.1/inlucde/c++/backwards
          c:/MinGW/lib/gcc/mingw32/4.8.1/inlucde/c++/wingw32
          ")
     (*cygwin* "
          /usr/lib/gcc/i686-pc-cygwin/3.4.4/include/c++/i686-pc-cygwin
          /usr/lib/gcc/i686-pc-cygwin/3.4.4/include/c++/backward
          /usr/local/include
          /usr/lib/gcc/i686-pc-cygwin/3.4.4/include
          /usr/include
          /usr/lib/gcc/i686-pc-cygwin/3.4.4/../../../../include/w32api
          ")
     (*linux* "
          /usr/include
          /usr/lib/wx/include/gtk2-unicode-release-2.8
          /usr/include/wx-2.8
          /usr/include/gtk-2.0
          /usr/lib/gtk-2.0/include
          /usr/include/atk-1.0
          /usr/include/cairo
          /usr/include/gdk-pixbuf-2.0
          /usr/include/pango-1.0
          /usr/include/glib-2.0
          /usr/lib/glib-2.0/include
          /usr/include/pixman-1
          /usr/include/freetype2
          /usr/include/libpng14
          ")
     (t "")))

(setq my-clang-include-directories
      (mapcar (lambda (item)
                (concat
                 (string-rtrim (replace-regexp-in-string "^\s*" "" item))))
              (remove-if #'(lambda (item) (string-match-p "^\s*$" item))
                         (split-string clang-include-dir-str "\n"))))

(provide 'init-clang)
;;; init-clang.el ends here