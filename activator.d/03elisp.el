(require 'paredit)
(require 'highlight-parentheses)

(add-hook 'emacs-lisp-mode-hook 
          '(lambda () 
             (progn
               (paredit-mode)
               (highlight-parentheses-mode))))

