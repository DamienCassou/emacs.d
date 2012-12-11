(defun add-to-executable-path (path)
  (let ((expanded-path (expand-file-name path)))
    (add-to-list 'exec-path expanded-path)
    (setenv "PATH" (concat expanded-path ":" (getenv "PATH")))))

(add-to-executable-path "~/.emacs.d/el-get/notmuch")
(add-to-list 'load-path "~/.emacs.d/el-get/notmuch/emacs")

(require 'notmuch)
(notmuch-show "thread")
