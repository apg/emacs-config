;;; govet.el --- go vet for the Go source code

;;; Commentary:

;;   (require 'govet)
;;
;; After this, type M-x govet on Go source code.
;;
;; Usage:
;;   C-x `
;;     Jump directly to the line in your code which caused the first message.
;;
;;   For more usage, see Compilation-Mode:
;;     http://www.gnu.org/software/emacs/manual/html_node/emacs/Compilation-Mode.html

;;; Code:
(require 'compile)

(defun go-vet-buffer-name (mode)
 "*Govet*")

(defun govet-process-setup ()
  "Setup compilation variables and buffer for `govet'."
  (run-hooks 'govet-setup-hook))

(define-compilation-mode govet-mode "govet"
  "Vet is a static analysis tools for Go source code."
  (set (make-local-variable 'compilation-scroll-output) nil)
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-process-setup-function)
       'govet-process-setup)
)

;;;###autoload
(defun govet ()
  "Run govet on the current file and populate the fix list. Pressing C-x ` will jump directly to the line in your code which caused the first message."
  (interactive)
  (compilation-start (concat "go vet " buffer-file-name)
                     'govet-mode))

(provide 'govet)

;;; govet.el ends here
