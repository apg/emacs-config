(setq rcirc-server-alist
      '(("irc.libera.chat"
         :port 6697

         :encryption tls
         :channels ("#racket"
                    "#lobsters" "#fennel"
                    "#openbsd" "#metabug"
                    "#scheme" "#atreus")
         :user-name "apg"
         :nick "apg")))

(add-hook 'rcirc-mode-hook
          (lambda ()
            (rcirc-track-minor-mode 1)))

(setq rcirc-time-format "%Y-%m-%d %H:%M ")
(setq rcirc-omit-responses '("PART" "QUIT"))
