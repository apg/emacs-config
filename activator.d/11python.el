(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

(add-hook 'python-mode-hook 
          '(lambda ()
             (setq whitespace-style '(line))
             (setq whitespace-line-column 77 )
             (set (make-local-variable 'show-trailing-whitespace) t)o
             (flyspell-prog-mode)))

(add-hook  'python-mode-hook 
           (lambda () 
             (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
