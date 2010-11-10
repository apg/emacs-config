;; I don't like tabs, but Java programmers sure do!
(require 'ant)

(add-hook 'java-mode-hook 
          '(lambda () 
             (progn
               (flyspell-mode t)
               (flyspell-prog-mode)
               (c-set-style "Ellemtel")
               (set (make-local-variable 'c-basic-offset) 2)
               (set (make-local-variable 'tab-width) 2)
               (set (make-local-variable 'show-trailing-whitespace) t)
               (set (make-local-variable 'indent-tabs-mode) t))))