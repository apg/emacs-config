;;

(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.phl$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.php5$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

(add-hook 'php-mode-hook 
          '(lambda () 
             (progn
               (flyspell-mode t)
               (c-set-style "Ellemtel")
               (set (make-local-variable 'show-trailing-whitespace) t)
               (set (make-local-variable 'c-basic-offset) 3))))
;;                               (flymake-mode t)
;;               (local-set-key "\C-cd" 'flymake-display-err-menu-for-current-line)