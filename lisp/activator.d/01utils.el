(defun dos2unix ()
  "Convert this entire buffer from MS-DOS text file format to UNIX."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "\r$" "" nil)
    (goto-char (1- (point-max)))
    (if (looking-at "\C-z")
        (delete-char 1))))

;;; sane window sizing...
(defun three-quarters-window ()
  "Resizes current window big"
  (interactive)
  (let ((size (- (truncate (* .75 (frame-height))) (window-height))))
    (if (> size 0)
        (enlarge-window size))))

(defun unfill-region (start end)
  "Unfills a region by refilling with `point-max` as fill-column"
  (interactive "r")
  (let ((fill-column (point-max)))
    (fill-region start end)))

(defun unfill-paragraph ()
  "Unfills a paragraph by refilling with `point-max` as fill-column"
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph)))

(defun font-lock-restart ()
  (interactive)
  (setq font-lock-mode-major-mode nil)
  (font-lock-fontify-buffer))

(defun hc/url-to-env (s)
  (interactive "sURL: ")
  (labels ((plist->alist (x a)
                         (if (null x)
                             a
                           (cond
                            ((null (cdr x))
                              (plist->alist (cdr x) (cons (list (car x)) a)))
                            (t (plist->alist (cddr x) (cons (list (car x) (cadr x)) a)))))))
    (let* ((u (url-generic-parse-url s))
           (pq (url-path-and-query u)))
      (kill-new  (mapconcat (lambda (x)
                              (concat (upcase (car x))
                                      "="
                                      (if (null (cdr x)) ""  (cadr x))))
                            (append (plist->alist (cdr (split-string (car pq) "/")) '())
                                    (url-parse-query-string (or (cdr pq) "")))

                            "\n")))))



(defvar do-not-edit-lines-to-search 5)

(defun do-not-edit ()
  "Put buffer in read only mode if it contains DO NOT EDIT within the first couple of lines"
  (interactive)
  (save-excursion
    (let ((p (point)))
      (forward-line do-not-edit-lines-to-search)
      (when (re-search-backward "DO NOT EDIT" nil t)
        (setq buffer-read-only t)))))

(add-hook 'find-file-hook 'do-not-edit)
