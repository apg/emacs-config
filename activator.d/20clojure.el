; http://wiki.unto.net/setting-up-clojure-and-slime

(add-hook 'clojure-mode-hook 
          '(lambda () 
             (progn
               (highlight-parentheses-mode)
               (paredit-mode))))
