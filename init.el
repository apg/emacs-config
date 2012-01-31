(setq activator-load-path (expand-file-name "~/.emacs.d/activator.d/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))

;; thingy.
(setq x-super-keysym 'meta)

(require 'activator)
(activator-start)
(put 'narrow-to-region 'disabled nil)
