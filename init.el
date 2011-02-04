(setq activator-load-path (expand-file-name "~/.emacs.d/activator.d/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))

(require 'activator)
(activator-start)
(put 'narrow-to-region 'disabled nil)
