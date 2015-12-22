;;; init-compile.el --- Summary:
;; This file is created for configure the compilation.

;;; Commentary:
;; Define the behivour of compilation.
;; The compilation configure for clang under windows.

  ; @see http://xugx2007.blogspot.com.au/2007/06/benjamin-rutts-emacs-c-development-tips.html
  (setq compilation-window-height 8)
  (setq compilation-finish-function
        (lambda (buf str)
          (if (string-match "exited abnormally" str)
              ;;there were errors
              (message "compilation errors, press C-x ` to visit")
            ;;no errors, make the compilation window go away in 0.5 seconds
            (when (string-match "*compilation*" (buffer-name buf))
              ;; @see http://emacswiki.org/emacs/ModeCompile#toc2
              ;; (bury-buffer "*compilation*")
              ;; (winner-undo)
              ;; FIXME: just delete the *compilation* window
              (delete-other-windows-vertically)
              (message "COMPILATION SUCCEEDED!")
              ))))

(defun my-clang-compile4windows ()
  (setq compile-command "clang -o a.exe -Ic:/DXSDK/include -Lc:/DXSDK/lib \"-Id:/Program\\ Files/Microsoft\\ Visual\\ Studio/VC98/INCLUDE\" \"-Id:/Program\\ Files/Microsoft\\ Visual\\ Studio/VC98/MFC/INCLUDE\" \"-Id:/Program\\ Files/Microsoft\\ Visual\\ Studio/VC98/ALT/INCLUDE\" \"-Lc:/DXSDK/lib -Ld:/Program\\ Files/Microsoft\\ Visual\\ Studio/VC98/Lib\" \"-Ld:/Program\\ Files/Microsoft\\ Visual\\ Studio/VC98/MFC/Lib\" -lddraw -ldinput -ldinput8 -ldsound -ld3dim -ldxguid -lkernel32 -lgdi32 -luser32 -lwinspool -lcomdlg32 -ladvapi32 -lshell32 -lole32 -loleaut32 -luuid -lodbc32 -lodbccp32 -lstdc++"))

(defun my-clang-compile-allfiles-windows (command &optional comint)
  (interactive
   (list
    (let* (
           (compile-command (concat "clang -o a.exe " (mapconcat (lambda (x) (format "%s" x)) (directory-files (expand-file-name "./") nil ".\\(cpp\\|c\\)$") " ") " " "-Ic:/DXSDK/include -Lc:/DXSDK/lib \"-Id:/Program\\ Files/Microsoft\\ Visual\\ Studio/VC98/INCLUDE\" \"-Id:/Program\\ Files/Microsoft\\ Visual\\ Studio/VC98/MFC/INCLUDE\" \"-Id:/Program\\ Files/Microsoft\\ Visual\\ Studio/VC98/ALT/INCLUDE\" \"-Lc:/DXSDK/lib -Ld:/Program\\ Files/Microsoft\\ Visual\\ Studio/VC98/Lib\" \"-Ld:/Program\\ Files/Microsoft\\ Visual\\ Studio/VC98/MFC/Lib\" -lddraw -ldinput -ldinput8 -ldsound -ld3dim -ldxguid -lkernel32 -lgdi32 -luser32 -lwinspool -lcomdlg32 -ladvapi32 -lshell32 -lole32 -loleaut32 -luuid -lodbc32 -lodbccp32 -lstdc++ -lwinmm"))
           (command (eval compile-command)))
      (if (or compilation-read-command current-prefix-arg)
          (compilation-read-command command)
        command))
    (consp current-prefix-arg)))
  (unless (equal command (eval compile-command))
    (setq compile-command command))
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (setq-default compilation-directory default-directory)
  (compilation-start command comint))

(defun my-vc-compile-allfiles-windows (command &optional comint)
  (interactive
   (list
    (let* (
           (compile-command "cl *.cpp /MD /Ox /Ot /W3 /c /EHsc")
           (command (eval compile-command)))
      (if (or compilation-read-command current-prefix-arg)
          (compilation-read-command command)
        command))
    (consp current-prefix-arg)))
  (unless (equal command (eval compile-command))
    (setq compile-command command))
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (setq-default compilation-directory default-directory)
  (compilation-start command comint)
  ;; (compilation-start link-command comint)
  )
(defvar link-command nil)
(defun my-vc-link-allfiles-windows (command &optional comint)
  (interactive
   (list
    (let* (
           (link-command "link /OUT:a.exe *.obj *.res /SUBSYSTEM:WINDOWS /MACHINE:X86 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib winmm.lib ddraw.lib dsound.lib dinput.lib dinput8.lib dxguid.lib")
           (command (eval link-command)))
      (if (or compilation-read-command current-prefix-arg)
          (compilation-read-command command)
        command))
    (consp current-prefix-arg)))
  (unless (equal command (eval link-command))
    (setq link-command command))
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (setq-default compilation-directory default-directory)
  (compilation-start command comint)
  )
(defvar rc-command nil)
(defun my-vc-rc-allfiles-windows (command &optional comint)
  (interactive
   (list
    (let* (
           (rc-command "rc /l 0x804 *.rc")
           (command (eval rc-command)))
      (if (or compilation-read-command current-prefix-arg)
          (compilation-read-command command)
        command))
    (consp current-prefix-arg)))
  (unless (equal command (eval rc-command))
    (setq rc-command command))
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (setq-default compilation-directory default-directory)
  (compilation-start command comint)
  )



(defun my-clang-compile4mac ()
  (setq compile-command "clang "))

(cond
 (*win32*
  (progn
    (add-hook 'c-mode-common-hook 'my-clang-compile4windows)
    ))
 (*is-a-mac*
  (progn
    (add-hook 'c-mode-common-hook 'my-clang-compile4mac)
    ))
 )

(provide 'init-compile)

;;; init-compile.el ends here


