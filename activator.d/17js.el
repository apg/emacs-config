;; requires
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/coffee-mode"))
(require 'coffee-mode)

(autoload 'js2-mode "js2" nil t)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile$" . coffee-mode))

(add-hook 'js2-mode-hook 
          '(lambda ()
             (flyspell-prog-mode)))

(setq js2-auto-indent-flag t
      js2-indent-on-enter-key t
      js2-mode-escape-quotes t
      js2-basic-offset 2)

(add-hook 'coffee-mode-hook
          '(lambda ()
             (set (make-local-variable 'tab-width) 2)))