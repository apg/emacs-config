(defun apg/text-focus-mode ()
  (interactive)
  (linum-mode 0)
  (blink-cursor-mode)
  (auto-fill-mode 1)
  (visual-line-mode 1)
  (set (make-local-variable 'line-spacing) 5)
  (set (make-local-variable 'truncate-lines) nil)
  (set (make-local-variable 'hl-line-mode) nil)
  (setq buffer-face-mode-face '(:family "IBM Plex Mono" :height 150))
  (buffer-face-mode))

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'writegood-mode)

(add-hook 'markdown-mode 'apg/text-focus-mode)

(add-hook 'fundamental-mode 'flyspell-mode)

(setq markdown-css-paths (list (expand-file-name "~/.emacs.d/markdown-work.css")))
