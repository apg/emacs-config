;;; ack related stuff

(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

(if (file-exists-p (expand-file-name "~/bin/ack"))
    (setq ack-executable (expand-file-name "~/bin/ack"))
  (setq ack-executable (executable-find "ack")))