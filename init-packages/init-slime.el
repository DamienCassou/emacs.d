(setq inferior-lisp-program "sbcl")
;(setq inferior-lisp-program "~/.emacs.d/ibcl")
(require 'slime)
(slime-setup '(slime-fancy slime-asdf slime-hyperdoc))

(define-key (eval 'slime-mode-map) (kbd "C-c c") (lambda (&optional arg)
     "Copy/past the previous sexp to the slime buffer"
     (interactive "p")
     (kmacro-exec-ring-item (quote
			     ([67108896 134217730 134217847 24 111 25 return 24 111 134217734 134217734] 0 "%d")) arg)))
