;; irc related stuff here.

;;; figure out how to get notifications..
(require 'growl)

(setq rcirc-default-nick "apgwoz"
      rcirc-default-user-name "apgwoz"
      rcirc-default-user-full-name "Andrew Gwozdziewycz"
      rcirc-prompt "%n> "; list nick name
      rcirc-fill-prefix "  "
      rcirc-time-format "%H:%M"
      rcirc-keywords '("andrewg" "apgwoz" "ap9" "apgw")
      rcirc-buffer-maximum-lines 8192)

(setq rcirc-startup-channels-alist
      '(("\\.freenode\\.net$" "#autonomo.us" "#emacs" "#rcirc" "#scheme")
        ("\\.sixapart.com$" "#mt" "#mt-talk" "#6a" "#6aservices")))

(setq rcirc-authinfo
      (with-temp-buffer
        (insert-file-contents-literally "~/.rcirc-authinfo")
        (read (current-buffer))))

(add-hook 'rcirc-mode-hook
	  '(lambda ()
           (flyspell-mode t)
	    (rcirc-track-minor-mode 1)
           (set (make-local-variable 'scroll-conservatively) 8192)))



;; fonts
(defface rcirc-nick-in-message '((t (:background "lemon chiffon")))
  "My nick when mentioned by others.")
(defface rcirc-my-nick '((t (:foreground "purple")))
  "My own nick for rcirc.")
(defface rcirc-track-nick '((t (:inherit rcirc-my-nick)))
  "The face used indicate activity directed at you.")
(defface rcirc-nick-in-message-full-line '((t ()))
  "The face used emphasize the entire message when your nick is mentioned.")
(defface rcirc-track-keyword '((t (:inherit bold)))
  "The face used indicate activity directed at you.")
(defface rcirc-prompt '((t (:foreground "orchid")))
  "My prompt for rcirc.")

;; commands
(eval-after-load 'rcirc
  '(defun-rcirc-command reconnect (arg)
     "Reconnect the server process."
     (interactive "i")
     (unless process
       (error "There's no process for this target"))
     (let* ((server (car (process-contact process)))
            (port (process-contact process :service))
            (nick (rcirc-nick process))
            (channels)
            (query-buffers))
       (dolist (buf (buffer-list))
         (with-current-buffer buf
           (when (eq process (rcirc-buffer-process))
             (remove-hook 'change-major-mode-hook
                          'rcirc-change-major-mode-hook)
             (if (rcirc-channel-p rcirc-target)
                 (setq channels (cons rcirc-target channels))
               (setq query-buffers (cons buf query-buffers))))))
       (delete-process process)
       (rcirc-connect server port nick
                      rcirc-default-user-name
                      rcirc-default-user-full-name
                      channels))))

(add-hook 'rcirc-print-hooks
          '(lambda (process sender response target text)
             (let ((current-nick (rcirc-nick process)))
               (when (and (string-match current-nick text)
                          (not (string= sender current-nick)))
                 (growl "You were mentioned" text)))))