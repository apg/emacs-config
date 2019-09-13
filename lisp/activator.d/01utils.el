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
