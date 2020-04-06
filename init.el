(require 'cl)

(add-to-list 'load-path (expand-file-name
                         (concat user-emacs-directory "lisp/")))

(setq activator-load-path (expand-file-name
                           (concat user-emacs-directory
                                   "lisp/activator.d/")))

(if (require 'package nil t)
    (progn
      (add-to-list 'package-archives
                   '("melpa" . "https://melpa.org/packages/"))

      (defvar apg-packages '(magit
                             markdown-mode
                             lua-mode
                             yaml-mode
                             highlight-parentheses
                             geiser
                             go-mode
                             git-gutter
                             git-link
                             gist
                             lsp-mode
                             lsp-ui
                             company-lsp))
      (package-initialize)
      (dolist (p apg-packages)
       (when (not (package-installed-p p))
         (package-install p))))

      (warn "NO package.el!"))

(dolist (p '("paredit.el"
             "writegood-mode.el"
             "fennel-mode.el"
             "rust-mode.el"
             "activator.el"
             "graphviz-dot-mode.el"))
  (load-file (expand-file-name
              (concat user-emacs-directory "lisp/" p))))

(activator-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c05b3e1761ba96b8169e62fd9c1a7359844a4c274f6879b6105984719f5fe8d7" default)))
 '(git-gutter:lighter " GG")
 '(package-selected-packages
   (quote
    (flycheck company-lsp lsp-ui lsp-mode lua-mode git-link yaml-mode markdown-mode magit highlight-parentheses go-mode git-gutter gist geiser darktooth-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
