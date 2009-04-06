(add-hook 'text-mode-hook 
          '(lambda ()
             (flyspell-mode t)))

(add-hook 'fundamental-mode
          '(lambda ()
             (flyspell-mode t)))