;; Redefines info-other-window to use features of `info'
(defun info-other-window (&optional file-or-node buffer)
  "Like `info' but show the Info buffer in another window."
  (interactive (list
                (if (and current-prefix-arg (not (numberp current-prefix-arg)))
                    (read-file-name "Info file name: " nil nil t))
                (if (numberp current-prefix-arg)
                    (format "*info*<%s>" current-prefix-arg))))
  (info-setup file-or-node (switch-to-buffer-other-window (or buffer "*info*"))))
