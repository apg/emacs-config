(require 'git-gutter)

;;; paredit autoload
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)

;;; Stolen from technomancy
(defun apg/paredit-no-space ()
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil))))

;;; general program mode hooks
(defvar apg/prog-mode-hooks '(highlight-parentheses-mode
                              git-gutter-mode
                              flyspell-prog-mode
                              (lambda ()
                                (setq tab-width 2)
                                (set (make-local-variable 'show-trailing-whitespace) t)
                                (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))))

(dolist (m apg/prog-mode-hooks)
  (add-hook 'prog-mode-hook m))

;;; lisp-mode
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)

;;; scheme
(add-hook 'scheme-mode-hook           'enable-paredit-mode)

(eval-after-load "geiser"
  '(progn
     (when (file-exists-p "/usr/local/racket/bin/racket")
       (setq geiser-racket-binary "/usr/local/racket/bin/racket"))
     (setq geiser-active-implementations '(racket guile chicken chibi))))


;;; fennel
(add-hook 'fennel-mode-hook 'enable-paredit-mode)


;;; go

;; basic custom
(add-hook 'go-mode-hook
          (lambda ()
            (set (make-local-variable 'c-basic-offset) 2)
            (set (make-local-variable 'tab-width) 2)
            (local-set-key (kbd "M-*") 'pop-tag-mark)))

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save nil t)))

(add-hook 'go-mode-hook
          (lambda ()
            (require 'lsp-mode)
            (require 'company-lsp)
            (setq lsp-enable-snippet nil)
            (setq lsp-enable-indentation t)
            (lsp)
            (local-set-key (kbd "M-?") 'company-complete)))

(eval-after-load "go-mode"
  '(progn
     (when (not (executable-find "gopls"))
       (warn "NO gopls found. `go get golang.org/x/tools/gopls"))
     (if (executable-find "goimports")
         (setq gofmt-command "goimports")
       (warn "NO goimports found. `go get golang.org/x/tools/cmd/goimports`"))))

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
    (add-hook 'erlang-mode-hook 'apg/paredit-no-space)
    (add-hook 'erlang-mode-hook 'enable-paredit-mode)

    (eval-after-load "erlang-mode"
      '(progn
         (define-key erlang-mode-map "{" 'paredit-open-curly)
         (define-key erlang-mode-map "}" 'paredit-close-curly)
         (define-key erlang-mode-map "[" 'paredit-open-bracket)
         (define-key erlang-mode-map "]" 'paredit-close-bracket)
         (define-key erlang-mode-map (kbd "RET")
           'reindent-then-newline-and-indent)

         (when (file-exists-p erlmode-el-path)
           (add-to-list 'load-path erlmode-el-path)
           (require 'erlmode-start))))))

;; common lisp / Slime
(let ((slime-helper-el (expand-file-name "~/quicklisp/slime-helper.el")))
  (if (file-exists-p slime-helper-el)
      (load slime-helper-el)
    (message "Woah there partner! Do you have the quicklisp slime-helper installed?")))

;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")



;;; autoload for fennel
(autoload 'fennel-mode "fennel-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode))

;; autoload for Rust
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; rust
(eval-after-load "rust-mode"
  '(add-hook 'rust-mode-hook 'rust-enable-format-on-save))

;; graphviz
(autoload 'graphviz-dot-mode "graphviz-dot-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))

(eval-after-load "graphviz-dot-mode"
  '(progn
     (setq graphviz-dot-indent-tab-width 2)
     (setq graphviz-dot-auto-indent-on-newline t)
     (setq graphviz-dot-auto-indent-on-braces nil)
     (setq graphviz-dot-auto-indent-on-semi t)))

;; json
(eval-after-load "js-mode"
  '(progn
     (when (not (executable-find "jq"))
       (warn "NO jq found. Get it here: https://stedolan.github.io/jq/"))))

(defun apg/json-format ()
  (interactive)
  (let* ((current (current-buffer))
         (command (format "%s ." (executable-find "jq")))
         (exit-status 0)
         (output (with-temp-buffer
                   (let ((temp-buffer (current-buffer)))
                     (with-current-buffer current
                       (setq exit-status (call-shell-region (point-min) (point-max) command nil temp-buffer))))
                   (buffer-string))))
    (if (zerop exit-status)
        (progn
          (delete-region (point-min) (point-max))
          (insert output))
      (let ((jq-output (or (get-buffer "*jq format output*")
                            (generate-new-buffer "*jq format output*"))))
        (with-current-buffer jq-output
          (insert output)
          (display-buffer jq-output))))))

(add-hook 'js-mode-hook
          '(lambda ()
             (when (string-suffix-p ".json" (buffer-file-name))
               (add-hook 'before-save-hook
                         'apg/json-format nil t))))
