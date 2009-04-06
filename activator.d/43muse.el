(add-to-list 'auto-mode-alist '("\\.muse$" . muse-mode))

(add-hook 'muse-mode-hook 
          '(lambda () 
             (flyspell-mode t)))
