(add-to-list 'auto-mode-alist '("\\.ddl$" . sql-mode))
(add-to-list 'auto-mode-alist '("\\.sql$" . sql-mode))

(add-hook 'sql-mode-hook 
          '(lambda ()
             (flyspell-mode t)))