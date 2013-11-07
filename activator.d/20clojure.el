(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/clojure-mode"))
(require 'clojure-mode)


(add-hook 'clojure-mode-hook
          '(lambda ()
             (progn
               (set (make-local-variable 'show-trailing-whitespace) t)
               (highlight-parentheses-mode)
               (paredit-mode))))

(add-hook  'clojure-mode-hook
           (lambda ()
             (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
