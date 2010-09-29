; http://wiki.unto.net/setting-up-clojure-and-slime

(if (file-exists-p (expand-file-name "~/local/clojure"))
    (progn
      (add-to-list 'load-path (expand-file-name "~/local/clojure-mode"))
      (add-to-list 'load-path 
                   (expand-file-name "~/local/swank-clojure/src/emacs"))
      (add-to-list 'load-path (expand-file-name "~/local/slime"))

      (require 'clojure-mode)
      (require 'slime)

      (require 'paredit)

      (add-hook 'clojure-mode-hook 
                '(lambda () 
                   (progn
                     (highlight-parentheses-mode)
                     (paredit-mode))))

      (eval-after-load "slime" (slime-setup '(slime-repl)))

      (slime-setup)))
