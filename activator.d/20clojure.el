(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/clojure-mode"))
(require 'clojure-mode)


(add-hook 'clojure-mode-hook 
          '(lambda () 
             (progn
               (highlight-parentheses-mode)
               (paredit-mode))))
