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

(setq rcirc-omit-responses '("PART" "QUIT"))
