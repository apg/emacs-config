(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(add-hook 'org-mode-hook 
          '(lambda () 
             (flyspell-mode t)))

