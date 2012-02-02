(let ((notmuch-path "~/.emacs.d/el-get/notmuch/"))
  (add-to-list 'exec-path notmuch-path)
  (setenv "PATH" (concat (getenv "PATH") ":" notmuch-path)))
