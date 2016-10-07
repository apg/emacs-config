(require 'cl)

(add-to-list 'load-path (expand-file-name "~/emacs.d/lisp/"))
(setq activator-load-path (expand-file-name "~/.emacs.d/lisp/activator.d/"))

(if (require 'package nil t)
    (progn
      (warn "YOU ARE USING HTTP FOR MARMALADE. CHECK TO SEE IF THIS HAS CHANGED!")
      (add-to-list 'package-archives
                   '("marmalade" . "http://marmalade-repo.org/packages/"))

      (add-to-list 'package-archives
                   '("melpa" . "http://melpa.org/packages/"))

      (defvar apg-packages '(magit
                             markdown-mode
                             yaml-mode
                             highlight-parentheses
                             coffee-mode
                             geiser
                             go-mode
                             git-gutter
                             gist
                             darktooth-theme))
      (package-initialize)

      (dolist (p apg-packages)
	(when (not (package-installed-p p))
	  (package-install p))))
  (warn "NO package.el!"))

(load-file (expand-file-name "~/.emacs.d/site-lisp/git-link.el"))
(load-file (expand-file-name "~/.emacs.d/lisp/activator.el"))
(load-file (expand-file-name "~/.emacs.d/lisp/paredit.el"))
(activator-start)

(custom-set-variables
 '(custom-safe-themes
   (quote
    ("c05b3e1761ba96b8169e62fd9c1a7359844a4c274f6879b6105984719f5fe8d7" default)))
 '(git-gutter:lighter " GG"))
