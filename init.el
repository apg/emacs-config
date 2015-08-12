(require 'cl)

(add-to-list 'load-path (expand-file-name "~/emacs.d/lisp/"))
(setq activator-load-path (expand-file-name "~/.emacs.d/lisp/activator.d/"))

(if (require 'package nil t)
    (progn
      (warn "YOU ARE USING HTTP FOR MARMALADE. CHECK TO SEE IF THIS HAS CHANGED!")
      (add-to-list 'package-archives
                   '("marmalade" . "https://marmalade-repo.org/packages/"))

      (add-to-list 'package-archives
                   '("melpa" . "http://melpa.org/packages/"))

      (defvar apg-packages '(paredit
                             magit
                             markdown-mode
                             yaml-mode
                             highlight-parentheses
                             coffee-mode
                             geiser
                             go-mode
                             git-gutter
                             gist))
      (package-initialize)

      (dolist (p apg-packages)
	(when (not (package-installed-p p))
	  (package-install p))))
  (warn "NO package.el!"))

(load-file (expand-file-name "~/.emacs.d/lisp/activator.el"))
(activator-start)
