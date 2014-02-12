;; scheme related things

(require 'paredit)
(require 'highlight-parentheses)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/geiser/elisp"))
(require 'geiser)

(setq geiser-impl-installed-implementations '(guile))


;;(add-to-list 'load-path (expand-file-name "~/src/swank-chicken"))
;;(autoload 'chicken-slime "chicken-slime" "SWANK backend for Chicken" t)

;;(setq slime-csi-path "/usr/local/bin/csi")

(add-hook 'scheme-mode-hook 
          '(lambda () 
             (progn
               (paredit-mode)
               (highlight-parentheses-mode))))

