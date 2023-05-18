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

(global-font-lock-mode t)

(show-paren-mode t)
(column-number-mode t)

;; disabled stuff
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(setq user-full-name "Andrew Gwozdziewycz")
(setq user-mail-address "me@apgwoz.com")

;; IBM Plex Preferred.
(when (member "IBM Plex Mono" (font-family-list))
  (set-frame-font "IBM Plex Mono-11" t t))

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


(setq apg/personal-notes-file (expand-file-name "~/Documents/notes.org"))
(setq apg/postits-dir (expand-file-name "~/Dropbox/Notes/postits/"))


(setq apg/browser-prefs (list '("firefox" . browse-url-firefox)
                              '("chromium" . browse-url-chromium)
                              '("chrome" . browse-url-chrome)))

(defun apg/best-browser (orelse)
  (let ((found nil)
        (cur (car apg/browser-prefs))
        (rest (cdr apg/browser-prefs)))
    (while (and (not found) rest)
      (when (executable-find (car cur))
        (setq found (cdr cur)))
      (setq cur (car rest))
      (setq rest (cdr rest)))
    (if found found orelse)))

(setq browse-url-browser-function
      (case system-type
        ((darwin) 'browse-url-default-macosx-browser)
        (t (apg/best-browser 'eww-browse-url))))

(setq browse-url-temp-dir (expand-file-name "~/Downloads"))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;;; don't disable this. it's useful!
(put 'narrow-to-region 'disabled nil)
