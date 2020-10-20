(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)

(ido-mode t)

;;; get rid of tabs
(setq default-tab-width 7)
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

(setq scroll-setp 1)

;;; typing yes? blargh.
(defalias 'yes-or-no-p 'y-or-n-p)
(setq blink-matching-paren t)

(add-to-list 'backup-directory-alist (cons "." "~/.backups"))

(setq browse-url-browser-function 'browse-url-firefox)

(global-font-lock-mode t)

(show-paren-mode t)
(column-number-mode t)

;; disabled stuff
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(setq user-full-name "Andrew Gwozdziewycz")
(setq user-mail-address "me@apgwoz.com")

(global-hl-line-mode +1)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;;; No more extra frames for ediff!
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; highlight parentheses colors
(eval-after-load "highlight-parentheses"
  '(setq hl-paren-colors
         '("#8f8f8f" "orange1" "yellow1" "greenyellow" "green1"
           "springgreen1" "cyan1" "slateblue1" "magenta1" "purple")))


(setq apg/personal-notes-file (expand-file-name "~/Dropbox/Notes/notes.org"))
(setq apg/work-notes-file (expand-file-name "~/Dropbox/Notes/heroku/notes.org"))
(setq apg/postits-dir (expand-file-name "~/Dropbox/Notes/postits/"))


(setq browse-url-browser-function
      (case system-type
          ((darwin) 'browse-url-default-macosx-browser)
        ((gnu/linux) 'browse-url-firefox)
        ((berkeley-unix) 'browse-url-firefox)
        (t eww-browse-url)))


(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;;; don't disable this. it's useful!
(put 'narrow-to-region 'disabled nil)
