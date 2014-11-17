;;; paredit autoload
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)


;;; Stolen from technomancy
(defun apg-paredit-no-space ()
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil))))

;;; general program mode hooks
(defvar apg-prog-mode-hooks '(highlight-parentheses-mode
                              flyspell-prog-mode
                              (lambda ()
                                (set (make-local-variable 'show-trailing-whitespace) t)
                                (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))))

(dolist (m apg-prog-mode-hooks)
  (add-hook 'prog-mode-hook m))

;;; lisp-mode
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)

;;; scheme
(add-hook 'scheme-mode-hook           'enable-paredit-mode)
(eval-after-load "geiser"
  '(progn
     (setq geiser-imple-installed-implementations '(guile))))

;;; go
(add-hook 'go-mode-hook
          (lambda ()
            (set (make-local-variable 'c-basic-offset) 2)
            (set (make-local-variable 'tab-width) 2)
            (add-hook 'before-save-hook 'gofmt nil t)))
(eval-after-load "go-mode"
  '(progn
     (if (executable-find "goimports")
         (setq gofmt-command "goimports")
       (warn "NO goimports found. `go get github.com/bradfitz/goimports`"))))

;;; css
(eval-after-load "css-mode"
  '(setq css-indent-offset 1))

;;; erlang
(add-to-list 'ido-ignore-files ".beam")

(let ((erlang-el-path "/usr/local/lib/erlang/lib/tools-2.6.12/emacs")
      (erlmode-el-path (expand-file-name "~/.emacs.d/site-lisp/erlmode")))
  (when (file-exists-p erlang-el-path)
    (add-to-list 'load-path erlang-el-path)
    (autoload 'erlang-mode "erlang" "erlang" t)
    (add-to-list 'auto-mode-alist '("\\.erl$" . erlang-mode))
    (add-to-list 'auto-mode-alist '("^rebar.config$" . erlang-mode))
    (add-hook 'erlang-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
    (add-hook 'erlang-mode-hook 'apg-paredit-no-space)
    (add-hook 'erlang-mode-hook 'enable-paredit-mode)

    (eval-after-load "erlang-mode"
      '(progn
         (define-key erlang-mode-map "{" 'paredit-open-curly)
         (define-key erlang-mode-map "}" 'paredit-close-curly)
         (define-key erlang-mode-map "[" 'paredit-open-bracket)
         (define-key erlang-mode-map "]" 'paredit-close-bracket)
         (define-key erlang-mode-map (kbd "C-M-h") 'backward-kill-word)
         (define-key erlang-mode-map (kbd "RET")
           'reindent-then-newline-and-indent)

         (when (file-exists-p erlmode-el-path)
           (add-to-list 'load-path erlmode-el-path)
           (require 'erlmode-start))))))
