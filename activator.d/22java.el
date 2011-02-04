;; I don't like tabs, but Java programmers sure do!
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/ant-el"))

(require 'ant)

(autoload 'jtags-mode "jtags" "Toggle jtags mode." t)
(autoload 'jtags-extras "jtags-extras" "Load jtags-extras.")

(add-hook 'java-mode-hook '(lambda () 
                             (jtags-mode 1)
                             (jtags-extras)))
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