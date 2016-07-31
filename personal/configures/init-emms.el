(if (not (file-exists-p (expand-file-name "emms" my-personal-dir)))
  (make-directory (expand-file-name "emms" my-personal-dir)))

(require 'emms-setup)
(emms-standard)
(emms-default-players)
;; Show the current track each time EMMS
;; starts to play a track with "NP : "
(add-hook 'emms-player-started-hook 'emms-show)
(setq emms-show-format "NP: %s")
;; When asked for emms-play-directory,
;; always start from this one
(setq emms-source-file-default-directory (expand-file-name "Music" my-personal-dir))
(provide 'init-emms)
