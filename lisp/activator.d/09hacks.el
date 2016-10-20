;;; tramp has a bug that was fixed in 25.1, in which it'll make ssh
;;; try to resolve host.does.not.exist, which can be problematic with
;;; some DNS servers:
;;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=20015
(when (< (string-to-number emacs-version) 25.1)
  (setq tramp-ssh-controlmaster-options ""))
