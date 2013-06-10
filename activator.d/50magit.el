;;; Autoloads for magit

(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/magit"))
(require 'magit)

(global-set-key (kbd "C-c g") 'magit-status)
