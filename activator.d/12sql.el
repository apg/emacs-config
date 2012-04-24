(add-to-list 'auto-mode-alist '("\\.ddl$" . sql-mode))
(add-to-list 'auto-mode-alist '("\\.sql$" . sql-mode))

(defun sql-send-region-and-switch (beg end)
  (interactive "r")
  (progn
    (sql-send-region beg end)
    (switch-to-buffer-other-window sql-buffer)))

(add-hook 'sql-mode-hook 
          '(lambda ()
             (local-set-key "\C-x \C-e" 'sql-send-region-and-switch)
             (flyspell-prog-mode)))

