(setq rcirc-server-alist
      '(("irc.freenode.net"
         :channels ("#hackandtell" "#racket"
                    "#lobsters" "#fennel"
                    "#openbsd" "#metabug")
         :user-name "apgwoz"
         :nick "_apg")))


(add-hook 'rcirc-mode-hook
          (lambda ()
            (rcirc-track-minor-mode 1)))

(setq rcirc-time-format "%Y-%m-%d %H:%M ")
(setq rcirc-omit-responses '("PART" "QUIT"))
