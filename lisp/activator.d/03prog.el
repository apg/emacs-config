(require 'git-gutter)

;;; paredit autoload
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code."
  t)

;;; eglot configuration.
(setq-default eglot-workspace-configuration
    '((:gopls .
        ((staticcheck . t)
         (matcher . "CaseSensitive")))))

(require 'eglot)
(define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
(define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-organize-imports)

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

;;; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
      `(,(expand-file-name
          (concat user-emacs-directory "lisp/snippets"))))
(yas-reload-all)

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
     (setq geiser-active-implementations '(racket chicken chibi))))


;;; gerbil
(autoload 'gerbil-mode "gerbil" "Gerbil editing mode." t)
(eval-after-load "gerbil"
  '(progn
     (require 'gambit)
     (add-hook 'inferior-scheme-mode-hook 'gambit-inferior-mode)
     (defvar gerbil-program-name
       (expand-file-name "~/local/bin/gxi")) ; Set this for your GERBIL_HOME
     (setq scheme-program-name gerbil-program-name)))


;;; go
(eval-after-load "go-mode"
  '(progn
     (when (not (executable-find "gopls"))
       (warn "NO gopls found. `go get golang.org/x/tools/gopls"))

     (let ((goimports (executable-find "goimports")))
       (if (not goimports)
           (warn "NO goimports found. `go get golang.org/x/tools/cmd/goimports")
         (setq gofmt-command goimports)))))


(add-hook 'go-mode-hook 'yas-minor-mode)
(add-hook 'go-mode-hook 'eglot-ensure)
(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook
          (lambda ()
            (set (make-local-variable 'c-basic-offset) 2)
            (set (make-local-variable 'tab-width) 2)
            (set (make-local-variable 'create-lockfiles) nil)

            ;; eglot-format-buffer is super slow.
            (add-hook 'before-save-hook 'gofmt-before-save)
            ;(add-hook 'before-save-hook #'eglot-format-buffer -10 t)
            ))

;;; sml
(add-hook 'sml-mode-hook
          (lambda ()
            (set (make-local-variable 'sml-indent-level) 2)
            (set (make-local-variable 'indent-tabs-mode) nil)))
(add-hook 'sml-mode-hook 'yas-minor-mode)
(setq sml-program-name "sml")

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
(add-hook 'fennel-mode-hook 'enable-paredit-mode)

;; autoload for Rust
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))

;; rust
(eval-after-load "rust-mode"
  '(add-hook 'rust-mode-hook 'rust-enable-format-on-save))

;; typescript
(eval-after-load "typescript-mode"
  (setq typescript-indent-level 2))

;; HCL / terraform
(autoload 'hcl-mode "hcl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.tf$" . hcl-mode))

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
