;;; Functions I use, that are good utilities.

;;; Usage: M-x dos2unix
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
(global-set-key "\C-x7" 'three-quarters-window)

;;; http://blog.tuxicity.se/?p=32
(defun google-region (beg end)
  "Google the selected region."
  (interactive "r")
  (browse-url (concat "http://www.google.com/search?ie=utf-8&oe=utf-8&q=" (buffer-substring beg end))))

(defun let-me-google-that-for-you (beg end)
  "Allows you to be a nice guy, and quickly lmgtfy the current region"
  (interactive "r")
  (kill-new (format "http://lmgtfy.com/?q=%s"
                    (url-hexify-string (buffer-substring beg end)))))
