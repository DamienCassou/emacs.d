(require 'gnus-art)
(require 'notmuch-labeler) ;; <- don't know why this is necessary

(add-to-list 'load-path "~/.emacs.d/notmuch-gmail")
(require 'notmuch-gmail)

(setq notmuch-labeler-folder-base "~/Mail/GmailTest")
