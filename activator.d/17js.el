;; requires
(autoload 'js2-mode "js2" nil t)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-hook 'js2-mode-hook 
          '(lambda ()
             (flyspell-mode t)))

(setq js2-auto-indent-flag 0
      js2-indent-on-enter-key 0
      js2-mode-escape-quotes 0
      js2-basic-offset 2)
