;; irc related stuff here.
;;
;; ~/.rcirc-authinfo is referenced which should look something like:
;; (("freenode" nickserv "username" "password")
;;  ("sixapart" nickserv "username2" "password2"))
;;
;; see the rcirc info pages for more info
;; 
;;; figure out how to get notifications..
(require 'notify)

(setq rcirc-default-nick "apgwoz"
      rcirc-default-user-name "apgwoz"
      rcirc-default-user-full-name "Andrew Gwozdziewycz"
      rcirc-prompt "%n> "; list nick name
      rcirc-fill-prefix "  "
      rcirc-time-format "%H:%M"
      rcirc-keywords '("andrewg" "apgwoz" "ap9" "apgw")
      rcirc-buffer-maximum-lines 8192)

(setq rcirc-startup-channels-alist
      '(("\\.freenode\\.net$" "#autonomo.us" "#emacs" "#clojure" "#clojure-web" 
         "#clojure-casual" "#hackandtell" "#nyhacker" "#nycpython" "#summeroflisp")
        ("\\.meetup.com$" "#dev")))


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
                 (notify (format "You were mentioned in %s" target) text)))))

;; (add-hook 'rcirc-receive-message-hooks
;;           '(lambda (process command sender args line)
;;              (message (format "process: %s" process))
;;              (message (format "sender: %s" sender))
;;              (message (format "args: %s" args))
;;              (message (format "line: %s" line))
;;              (message (format "command: %s" command))))


;(setq rcirc-receive-message-hooks '())

;; (add-hook 'rcirc-receive-message-hooks 
;;           '(lambda (process command sender args line)
;;              (let ((sp (format "%s" process))
;;                    (channel (car args)))
;;                (when (and (string-equal "#glpbacon" channel)
;;                           (string-equal "irc.freenode.net" sp)
;;                           (string-equal "JOIN" command))
;;                  (rcirc-send-message process "#glpbacon" (concat sender "!"))))))


(progn 
  (setq rcirc-print-hooks '())
  (setq rcirc-receive-hooks '())
  (setq rcirc-receive-message-hooks '()))

(add-hook 'rcirc-print-hooks 
          '(lambda (process sender response target text)
             (let ((sp (format "%s" process)))
               (when (and (string-equal "#glpbacon" target)
                          (string-equal "irc.freenode.net" sp)
                          (string-match "doctor andrew:" text))
                 (rcirc-send-message process "#glpbacon" (do-doctor (substring message 15)))))))

(defun init-buffer ()
  (let ((buf (get-buffer "*#glpbacon-doctor")))
    (if buf
        buf
      (let ((buf (get-buffer-create "*#glpbacon-doctor*")))
        (with-current-buffer buf
          (make-doctor-variables)
          buf)))))

(defun do-doctor (message)
  (let ((buf (init-buffer)))
    (with-current-buffer buf
      (delete-region (buffer-end -1) (buffer-end 1))
      (insert message)
      (let ((sent (doctor-readin)))
        (insert "\n\n")
        (doctor-doc sent)
        (backward-sentence 1)
        (buffer-substring (point) (buffer-end 1))))))
