(require 'lua-mode)

(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

(add-hook 'lua-mode-hook 
          '(lambda () 
             (progn
               (flyspell-prog-mode))
               (set (make-local-variable 'show-trailing-whitespace) t)))

(add-hook  'lua-mode-hook 
           (lambda () 
             (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
