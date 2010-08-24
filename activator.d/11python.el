(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

(add-hook 'python-mode-hook 
          '(lambda ()
             (whitespace-mode t)
             (setq whitespace-style '(line))
             (setq whitespace-line-column 77 )
             (flyspell-mode t)))