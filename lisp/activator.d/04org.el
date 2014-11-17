(eval-after-load "org-mode"
  '(progn
     (add-hook 'org-mode-hook 'flyspell-mode)

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

     (let ((notes-file (expand-file-name "~/Dropbox/Notes/notes.org")))
       (when (file-exists-p notes-file)
         (setq org-default-notes-file notes-file)))))
