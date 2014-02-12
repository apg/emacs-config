(require 'slime)
(eval-after-load "slime" (slime-setup '(slime-repl)))
(slime-setup)

(setq inferior-lisp-program "sbcl")
