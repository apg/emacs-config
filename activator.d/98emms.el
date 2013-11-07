(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/emms/lisp"))

;; this is totally lame defaults...
(require 'emms-setup)
(emms-standard)

(emms-default-players)

(global-set-key (kbd "<pause>") 'emms-pause)


