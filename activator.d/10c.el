(add-hook 'c-mode-hook 
          '(lambda () 
             (progn
               (flyspell-prog-mode)
               (set (make-local-variable 'show-trailing-whitespace) t))))

(add-hook  'c-mode-hook 
           (lambda () 
             (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
