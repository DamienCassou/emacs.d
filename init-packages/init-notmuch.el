(setenv "NOTMUCH_CONFIG" (expand-file-name "~/.notmuch-config-gmail"))

(add-to-executable-path "~/.emacs.d/el-get/notmuch")
(add-to-list 'load-path "~/.emacs.d/el-get/notmuch-labeler")
(add-to-list 'load-path "~/.emacs.d/notmuch-gmail")

(require 'notmuch-labeler)
(require 'notmuch-gmail)
