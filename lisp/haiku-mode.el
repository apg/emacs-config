(defface haiku-success-face
  '((t :inherit success))
  :group 'haiku-mode)

(defface haiku-error-face
  '((t :inherit warning))
  :group 'haiku-mode)


(defvar haiku-highlight-keywords
  '(("~~" . font-lock-comment-delimiter-face)
    haiku-highlight)
  "font-lock-defaults for the mode")

(defconst haiku-count-syllables-negative
  '("cial" "tia" "cius" "cious" "giu" "ion" "iou" "sia$" ".ely$"))

(defconst haiku-count-syllables-positive
  '("ia" "riet" "dien" "iu" "io" "li"
    "[aeiouym]bl$"
    "[aeiou]\\{3\\}"
    "^mc"
    "ism$" ; isms
    "\\([^aeiouy]\\)\\1l$" ; middle twiddle battle bottle, etc
    "[^l]lien" ; alien, salient, but not lien, or ebbuillient
    "^coa[dglx]" ; exception for words coadjutor coagulable coagulate
                 ; coalesce coalescent coalition coaxial
    "[^gq]ua[^aeiou]"
    "dnt$"))  ; couldn't

(defun haiku-count-word-syllables (word)
  (let ((word (downcase word))
        (word (replace-regexp-in-string "'" "" word))
        (word (replace-regexp-in-string "e$" "" word))
        (vowgrouplen (length (remove-if-not '(lambda (x) (> (length x) 0))
                               (split-string word "[^aeiouy]+")))))
    (if (= (length word) 1)
        1
      (progn
        (let ((pluses (reduce '(lambda (count thing)
                                 (if (string-match-p thing word)
                                     (1+ count)
                                   count))
                              haiku-count-syllables-positive
                              :initial-value 0))
              (minuses (reduce '(lambda (count thing)
                                  (if (string-match-p thing word)
                                      (1- count)
                                    count))
                               haiku-count-syllables-negative
                               :initial-value 0)))
          (or (+ pluses minuses vowgrouplen) 1))))))

(defun haiku-count-line-syllables (line)
  (reduce '(lambda (count word)
             (+ count (haiku-count-word-syllables word)))
          (remove-if-not '(lambda (x) (> (length x) 0))
                         (split-string line "[ ,.;:?#-]+"))
          :initial-value 0))

(defun haiku-font-lock-extend-region ()
  "Extend the search region to include an entire block of text."
  ;; Avoid compiler warnings about these global variables from font-lock.el.
  ;; See the documentation for variable `font-lock-extend-region-functions'.
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  (save-excursion
    (goto-char font-lock-beg)
    (let ((found (or (re-search-backward "~~" nil t) (point-min))))
      (goto-char font-lock-end)
      (when (re-search-forward "~~" nil t)
        (beginning-of-line)
        (setq font-lock-end (point)))
      (setq font-lock-beg found))))

(defun haiku-highlight (limit)
  (when (re-search-forward "~~" limit t) ;; get back to top of haiku
    (let ((sbeg (match-beginning 0))
          (send (match-end 0))
          (syllables '(5 7 5)))
      (re-search-forward "~~" limit t)
      ;; These might be the same, in which case, we don't have a valid
      ;; to match against haiku.
      (let ((ebeg (match-beginning 0))
            (eend (match-end 0)))
        (if (and (= sbeg ebeg)
                 (= send eend))
            (put-text-property sbeg send 'face 'haiku-success-face)
          ;; At this point, we can search through lines and ensure they
          ;; match the syllable pattern we're looking for.
          (progn
            (goto-char send)
            (setq syllables '(5 7 5))
            (setq is-haiku t)
            (while syllables
              (forward-line 1)
              (setq line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
              (if (= (haiku-count-line-syllables line) (car syllables))
                  (put-text-property (line-beginning-position) (line-end-position) 'face 'haiku-success-face)
                (progn
                  (put-text-property (line-beginning-position) (line-end-position) 'face 'haiku-error-face)
                  (setq is-haiku nil)))
              (setq syllables (cdr syllables)))

            (if (not is-haiku)
                (progn
                  (put-text-property sbeg send 'face 'haiku-error-face)
                  (put-text-property ebeg eend 'face 'haiku-error-face))
              (progn
                (put-text-property sbeg send 'face 'haiku-success-face)
                (put-text-property ebeg eend 'face 'haiku-success-face)))))))))

(define-derived-mode haiku-mode fundamental-mode "HAIKU"
  "major mode for editing Haiku"
  (set (make-local-variable 'font-lock-multiline) t)
  (setq font-lock-defaults '(haiku-highlight-keywords))
  (add-hook 'font-lock-extend-region-functions
            'haiku-font-lock-extend-region))

(provide 'haiku-mode)
