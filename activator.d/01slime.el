(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/slime"))

(require 'slime)
(eval-after-load "slime" (slime-setup '(slime-repl)))
(slime-setup)

(setq inferior-lisp-program "sbcl")
