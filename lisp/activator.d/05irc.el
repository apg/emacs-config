(setq rcirc-server-alist
      '("irc.freenode.net"
        :channels ("#emacs" "#hackandtell" "#racket" "#lobsters")
        :user-name "apgwoz"
        :nick "_apg"))

(add-hook 'rcirc-mode-hook
          (lambda ()
            (rcirc-track-minor-mode 1)))

(setq rcirc-omit-responses '("PART" "QUIT"))
