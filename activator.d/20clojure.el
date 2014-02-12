(require 'clojure-mode)
(require 'cider)

(add-hook 'clojure-mode-hook
          '(lambda ()
             (progn
               (set (make-local-variable 'show-trailing-whitespace) t))))

(add-hook  'clojure-mode-hook
           (lambda ()
             (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(add-hook 'clojure-mode 'highlight-paren-mode)
(add-hook 'clojure-mode 'paredit-mode)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

(setq cider-repl-indent-and-complete-symbol t)
(setq cider-repl-print-length 100)
