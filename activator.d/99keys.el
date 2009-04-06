;; keybindings and things

;; bind c-h backspace dammit!
(global-set-key "\C-h" 'delete-backward-char)

;; last keyboard macro should be something sane.. let's use f7
(define-key global-map [f7] 'call-last-kbd-macro)

;; whitespace has been annoying me lately..
(define-key global-map [f8] 'delete-trailing-whitespace)
