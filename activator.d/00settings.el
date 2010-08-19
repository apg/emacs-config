;;;; things that get set globally

(setq inhibit-startup-message t) 
(setq default-tab-width 7)
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

;; scroll one line at a time
(setq scroll-setp 1)

;; make all yes no prompts b y or n instead
(defalias 'yes-or-no-p 'y-or-n-p)

(setq blink-matching-paren t)

;; should be moved to an xml.el file
(setq sgml-basic-offset 3)
(setq nxml-child-indent 3)

(add-to-list 'backup-directory-alist (cons "." "~/.backups"))

(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "open")

(normal-erase-is-backspace-mode 0)
(global-font-lock-mode t)
(set-goal-column t) 

;; add /opt/local/bin to the end of the path
(add-to-list 'exec-path "/usr/local/bin")

;; make command work as meta key
(setq mac-command-modifier 'meta)

(show-paren-mode t)

(menu-bar-mode nil)

(column-number-mode t)

;; disabled stuff
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
