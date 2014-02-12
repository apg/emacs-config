;; I don't like tabs, but Java programmers sure do!
(require 'cl-reduce)

(add-to-list 'load-path (expand-file-name "~/Devel/moka-mode/lisp"))

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


(setq tags-table-exclude-patterns
      '("/ClojureDS/"
        "/Nim/"))

(setq find-java-repos "find ~/repos -name 'java' -type d | grep 'src/main'")
(setq excluded-project-regexps
      '("ClojureDS"
        "Nim"))

;; it'll be nice when moka can take care of this automagically!
(setq tags-table-list
      (let* ((candidates (split-string
                          (shell-command-to-string find-java-repos) "\n")))
        (remove-if-not
         '(lambda (candidate)
            (reduce '(lambda (accum new)
                       (and accum
                            (not (string-match-p new candidate))))
                    excluded-project-regexps
                    :initial-value t))
         candidates)))

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
