;;; emms
(let ((emms-dir (expand-file-name "~/.emacs.d/site-lisp/emms/lisp")))
  (add-to-list 'load-path emms-dir)
  (eval-and-compile
    (require 'emms-setup)
    (require 'emms-source-file)
    (require 'emms-source-playlist)
    (require 'emms-player-simple)
    (require 'emms-player-mplayer)
    (require 'emms-playlist-mode)
    (require 'emms-info))
  (setq emms-player-list (list emms-player-mplayer emms-player-mplayer-playlist))
  (setq emms-playlist-default-major-mode 'emms-playlist-mode))

;;; git-gutter custom
(custom-set-variables
 '(git-gutter:lighter " GG"))
