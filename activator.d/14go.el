(require 'go-mode-load)

(add-hook 'go-mode-hook
          (lambda ()
            (set (make-local-variable 'c-basic-offset) 2)
            (set (make-local-variable 'tab-width) 2)
            (set (make-local-variable 'show-trailing-whitespace) t)))