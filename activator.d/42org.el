(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(add-hook 'org-mode-hook 
          '(lambda () 
             (flyspell-mode t)))

(setq org-todo-keywords
       '((sequence "TODO" "INPROGRESS" "|" "DONE")
         (sequence "BUG" "VERIFY" "|" "FIXED" "WONTFIX")
         (sequence "|" "CANCELLED" "NEVER" "DELEGATED" "EXTERNAL")))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning) 
        ("BUG" . org-warning) 
        ("INPROGRESS" . org-block)
        ("VERIFY" . org-block)
        ("DONE" . org-done) 
        ("FIXED" . org-done) 
        ("WONTFIX" . org-done) 
        ("CANCELED" . org-table)
        ("NEVER" . org-table)
        ("DELEGATED" . org-table)
        ("EXTERNAL" . org-table)))

(setq org-hide-leading-stars t)

(setq org-agenda-files (list 
                        (expand-file-name "~/Dropbox/Notes") 
                        (expand-file-name "~/Dropbox/Notes/work") 
                        (expand-file-name "~/Dropbox/Notes/projects")))
