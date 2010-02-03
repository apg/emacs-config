; http://wiki.unto.net/setting-up-clojure-and-slime

(if (file-exists-p (expand-file-name "~/local/clojure"))
    (progn
      (add-to-list 'load-path (expand-file-name "~/local/clojure-mode"))
      (add-to-list 'load-path 
                   (expand-file-name "~/local/swank-clojure/src/emacs"))
      (add-to-list 'load-path (expand-file-name "~/local/slime"))

      (setq swank-clojure-jar-path 
            (expand-file-name "~/local/clojure/clojure-1.1.0.jar"))
      (setq swank-clojure-extra-classpaths
            (list (expand-file-name "~/local/clojure-contrib/clojure-contrib.jar")))

      (require 'clojure-mode)
      (require 'swank-clojure-autoload)
      (require 'slime)

      (eval-after-load "slime" (slime-setup '(slime-repl)))

      (slime-setup)))
