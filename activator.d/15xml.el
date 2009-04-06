;; html/xml related things

(setq sgml-basic-offset 3)
(setq nxml-child-indent 3)

(add-to-list 'auto-mode-alist '("\\.html?$" . sgml-mode))
(add-to-list 'auto-mode-alist '("\\.xhtml?$" . sgml-mode))
(add-to-list 'auto-mode-alist '("\\.xml?$" . sgml-mode))
(add-to-list 'auto-mode-alist '("\\.tpl$" . sgml-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl$" . sgml-mode))

;; maybe put something in here related to utf8?

(add-hook 'sgml-mode-hook 
          '(lambda () 
             (flyspell-mode t)))

