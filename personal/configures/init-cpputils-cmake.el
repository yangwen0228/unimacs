;;; init-cpputils-cmake.el --- Init for c/c++ clang dirs.
;;; Commentary:
;; All the dirs used in c/c++, flycheck, company-clang, company-irony.

(require 'cpputils-cmake)

(add-hook 'c-mode-common-hook
          (lambda ()
            (if (derived-mode-p 'c-mode 'c++-mode)
                (cppcm-reload-all))))
(require 'init-clang)
(setq cppcm-include-dirs my-clang-include-directories)
(setq cppcm-debug t)

;; redefine:
(defun cppcm-reload-all ()
  "reload and reproduce everything"
  (if cppcm-debug (message "cppcm-reload-all called"))
  (interactive)
  (let (dirs )
    (when buffer-file-name
      (setq cppcm-hash nil)
      (unless cppcm-hash
        (setq cppcm-hash (make-hash-table :test 'equal)))
      (setq dirs (cppcm-get-dirs))
      (if cppcm-debug (message "(cppcm-get-dirs)=%s" dirs))
      (cond
       ((car dirs)
        ;; looks normal, we find buid-dir and soure-dir
        ;; create info for other plugins at first
        ;; cppcm-include-dirs will be set here
        ;; create makefiles may fail if the executable does not exist yet
        (condition-case nil
            (progn
              ;; the order is fixed
              (cppcm-scan-info-from-cmake (nth 3 dirs) (nth 3 dirs) (nth 2 dirs))

              (cppcm-set-c-flags-current-buffer))
          (error
           (message "Failed to create Makefile for flymake. Continue anyway.")))
        )
       ((nth 1 dirs)
        ;; build-dir is found, but flags in build-dir need be created
        ;; warn user.
        (message "Please run cmake in %s at first" (nth 1 dirs))
        )
       (t
        (message "Build directory is missing! Create it and run cmake in it.")))
      )
    )

  (if cppcm-debug (message "cppcm-include-dirs=%s" cppcm-include-dirs))

  (when cppcm-include-dirs
    (setq company-clang-arguments
          (cppcm--fix-include-path
           (append cppcm-include-dirs
                   cppcm-preprocess-defines
                   cppcm-extra-preprocss-flags-from-user)))
    (if cppcm-debug (message "company-clang-arguments=%s" company-clang-arguments))

    ;; unlike auto-complete and company-mode, flycheck prefer make things complicated
    (setq flycheck-clang-include-path (delq nil
                                            (mapcar (lambda (str)
                                                      (cppcm--extract-include-directory str))
                                                    ac-clang-flags)))
    (if cppcm-debug (message "flycheck-clang-include-path=%s" flycheck-clang-include-path))

    (setq flycheck-clang-definitions (delq nil
                                           (mapcar (lambda (str)
                                                     (if (string-match "^-D *" str) (replace-regexp-in-string "^-D *" "" str)))
                                                   ac-clang-flags)))
    (if cppcm-debug (message "flycheck-clang-definitions=%s" flycheck-clang-definitions))

    ;; company-c-headers-path-system
    (setq company-c-headers-path-system flycheck-clang-include-path)
    (if cppcm-debug (message "company-c-headers-path-system=%s" company-c-headers-path-system))

    ;; irony compile-commands-path
    (if (fboundp 'irony-cdb-json-add-compile-commands-path)
        (irony-cdb-json-add-compile-commands-path cppcm-src-dir (concat cppcm-build-dir "compile_commands.json")))

    ;; set cc-search-directories automatically, so ff-find-other-file will succeed
    (add-hook 'ff-pre-find-hook
              '(lambda ()
                 (let (inc-dirs)
                   (setq inc-dirs (mapcar (lambda (item)
                                            (cppcm--extract-include-directory item))
                                          cppcm-include-dirs))
                   ;; append the directories into the cc-search-directories
                   ;; please note add-to-list won't insert duplicated items
                   (dolist (x inc-dirs) (add-to-list 'cc-search-directories x))
                   ))))
  (when (and cppcm-build-dir (file-exists-p (concat cppcm-build-dir "CMakeCache.txt")))
    (setq compile-command (concat "make -C \"" cppcm-build-dir "\"")))

  (run-hook-with-args 'cppcm-reload-all-hook))

(provide 'init-cpputils-cmake)
;;; init-cpputils-cmake.el ends here