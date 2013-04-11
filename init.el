(setq activator-load-path (expand-file-name "~/.emacs.d/activator.d/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))

;; thingy.
(setq x-super-keysym 'meta)

(require 'activator)
(activator-start)
(put 'narrow-to-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Dropbox/Notes/ok-work/tasks.org" "/home/apg/Dropbox/Notes/activator.org" "/home/apg/Dropbox/Notes/baby-photo-site.org" "/home/apg/Dropbox/Notes/cfdg-scm.org" "/home/apg/Dropbox/Notes/compacting-collector.org" "/home/apg/Dropbox/Notes/computer-science-for-toddlers.org" "/home/apg/Dropbox/Notes/deploy-notes.org" "/home/apg/Dropbox/Notes/h-and-t-conf.org" "/home/apg/Dropbox/Notes/habitual.org" "/home/apg/Dropbox/Notes/ideas.org" "/home/apg/Dropbox/Notes/interview-book.org" "/home/apg/Dropbox/Notes/interview-questions.org" "/home/apg/Dropbox/Notes/intro-clojure.org" "/home/apg/Dropbox/Notes/new-programming-language-ideas.org" "/home/apg/Dropbox/Notes/sql-cache.org" "/home/apg/Dropbox/Notes/tin.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
