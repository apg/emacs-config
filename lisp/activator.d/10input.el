;;; because technomancy laughed at me.
(defun shrug () (interactive) (insert "¯\\_(ツ)_/¯"))

(defun scrabble-emoji (begin end)
  "Silly slack fun for scrabble tiling text."
  (interactive "r")
  (let ((s (buffer-substring begin end)))
    (with-temp-buffer
      (insert (apply #'concat
                     (seq-map '(lambda (ch)
                                 (cond
                                  ((and (>= ch 97)
                                        (<= ch 122))
                                   (format ":scrabble-%c:" ch))
                                  (t (char-to-string ch))))
                              (downcase s))))
      (kill-region (point-min) (point-max)))))
