(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

(add-hook 'python-mode-hook 
          '(lambda ()
             (flyspell-mode t)))