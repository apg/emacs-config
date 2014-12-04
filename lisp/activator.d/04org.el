(defun apg/find-todays-insertion-point ()
  "Positions point at current heading in file"
  (interactive)
  (beginning-of-buffer)
  (if (search-forward (concat "[" (format-time-string "%F %a") "]") nil t)
      (progn
        (end-of-line)
        (insert "\n"))
    (progn
      (beginning-of-buffer)
      (re-search-forward "^$")
      (insert (concat "\n* [" (format-time-string "%F %a") "]")))))

(progn
  (require 'org-capture)

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

;;; setup the default notes file, if it exists
  (let ((notes-file (expand-file-name "~/Dropbox/Notes/notes.org")))
    (when (file-exists-p notes-file)
      (setq org-default-notes-file notes-file)))

  (setq org-capture-templates
        `(("t" "Todo" plain (file+function apg/personal-notes-file apg/find-todays-insertion-point)
           "** TODO %?\n  %i\n  %a")
          ("T" "(Work) Todo" plain (file+function apg/work-notes-file apg/find-todays-insertion-point)
           "** TODO %?\n  %i\n  %a")
          ("e" "Notes Entry" plain (file+function apg/personal-notes-file apg/find-todays-insertion-point)
           "** %?\n  %i\n")
          ("E" "(Work) Notes Entry" plain (file+function apg/work-notes-file apg/find-todays-insertion-point)
           "** %?\n  %i\n")
          ("p" "Postit Entry" plain (function
                                     (lambda ()
                                       (let ((filename
                                              (concat apg/postits-dir
                                                      (format-time-string "%F") ".org")))
                                         (set-buffer (or (get-file-buffer filename)
                                                         (get-buffer-create filename)))
                                         (set-visited-file-name filename)
                                         (end-of-buffer))))
           "* [[%c][%?]]\n%i\n"
           :empty-lines 1)))

  (setq org-publish-project-alist
        '(("postits-org"
           :base-directory "~/Dropbox/Notes/postits"
           :base-extension "org"
           :publishing-directory "/ssh:apg@peter.sigusr2.net:/var/www/postits.sigusr2.net"
           :publishing-function org-html-publish-to-html
           :section-numbers nil
           :auto-sitemap t
           :sitemap-filename "index.org"
           :sitemap-title "Postits"
           :sitemap-sort-files 'anti-chronologically
           :with-toc t
           :html-head "<link rel=\"stylesheet\"
                       href=\"../css/styles.css\" type=\"text/css\"/>"
           :html-preamble t
           :recursive t)
          ("postits-static"
           :base-directory "~/Dropbox/Notes/postits/static/"
           :base-extension "\\|css\\|js\\|jpg\\|gif\\|png"
           :publishing-directory "/ssh:user@host:/var/www/postits.sigusr2.net/static/"
           :recursive t
           :publishing-function org-publish-attachment)
          ("postits" :components ("postits-org" "postits-static")))))
