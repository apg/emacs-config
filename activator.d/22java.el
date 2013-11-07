;; I don't like tabs, but Java programmers sure do!

;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-eclim"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/ant-el"))

 (add-to-list 'load-path (expand-file-name "~/Devel/moka-mode/lisp"))
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/moka-mode"))

(autoload 'moka-mode "moka" "Toggle moka mode." t)
(add-hook 'java-mode-hook 'moka-mode)

(add-hook 'java-mode-hook 
          '(lambda () 
             (progn
               (flyspell-prog-mode)
               (c-set-style "Ellemtel")
               (set (make-local-variable 'c-basic-offset) 4)
               (set (make-local-variable 'tab-width) 4)
               (set (make-local-variable 'show-trailing-whitespace) t))))

(add-hook 'java-mode-hook 
           (lambda () 
             (add-to-list 'write-file-functions 'delete-trailing-whitespace)))



;; it'll be nice when moka can take care of this automagically!
(setq tags-table-list 
      (split-string 
       (shell-command-to-string "find ~/repos/ -name 'java' -type d | grep 'src/main'") "\n"))


(setq moka-cleanup-import-order-list 
      '("^com.knewton\\."
        "-"
        "^com\\."
        "-"
        "^net\\."
        "-"
        "^org\\."
        "-"
        "^java\\."
        "-"
        "^javax\\."
        "-"))

(setq moka-cleanup-organize-after-add-flag t) 
