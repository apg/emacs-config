(if (file-exists-p "/Users/me/scala/misc/scala-tool-support/emacs")
    (progn
      (add-to-list 'load-path "/Users/me/scala/misc/scala-tool-support/emacs")
      (require 'scala-mode-auto)))
