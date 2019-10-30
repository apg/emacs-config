(setq rcirc-server-alist
      '(("chat.freenode.net"
         :port 7000
         :encryption tls
         :channels ("#hackandtell" "#racket"
                    "#lobsters" "#fennel"
                    "#openbsd" "#metabug"
                    "#scheme")
         :user-name "apgwoz"
         :nick "_apg")))

(add-hook 'rcirc-mode-hook
          (lambda ()
            (rcirc-track-minor-mode 1)))

(setq rcirc-time-format "%Y-%m-%d %H:%M ")
(setq rcirc-omit-responses '("PART" "QUIT"))
