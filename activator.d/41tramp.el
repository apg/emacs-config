;; tramp things
(setq password-cache-expiry (* 60 60 8))

(setq tramp-backup-directory-alist backup-directory-alist)
(setq tramp-auto-save-directory "~/.emacs.d/auto-saves")