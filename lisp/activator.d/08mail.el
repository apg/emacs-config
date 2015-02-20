(let ((notmuch-el-path (expand-file-name "~/.emacs.d/site-lisp/notmuch")))
  (when (file-exists-p notmuch-el-path)
    (add-to-list 'load-path notmuch-el-path)
    (require 'notmuch)
    (require 'gnus-art)
    (add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)

    (define-key notmuch-search-mode-map "d"
      (lambda (&optional beg end)
        "mark thread as deleted"
        (interactive (notmuch-search-interactive-region))
        (notmuch-search-tag (list "+deleted" "-inbox") beg end)))

    (define-key notmuch-show-mode-map "d"
      (lambda ()
        "toggle deleted tag for message"
        (interactive)
        (if (member "deleted" (notmuch-show-get-tags))
            (notmuch-show-tag (list "-deleted"))
          (notmuch-show-tag (list "+deleted")))))

    (setq
     notmuch-crypto-process-mime t
     sendmail-program (expand-file-name "~/bin/msmtpq")
     message-send-mail-function 'message-send-mail-with-sendmail
     message-sendmail-f-is-evil nil
     message-sendmail-envelope-from 'header
     mail-specify-envelope-from t
     mail-envelope-from 'header)))
