;; requires
(autoload 'js2-mode "js2" nil t)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-hook 'js2-mode-hook 
          '(lambda ()
             (flyspell-prog-mode t)))

(setq js2-auto-indent-flag t
      js2-indent-on-enter-key t
      js2-mode-escape-quotes t
      js2-basic-offset 2)
