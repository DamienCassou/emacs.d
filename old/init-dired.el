;;; Configuring dired

(require 'dired-aux)
(require 'dired-x)

(defun dc:command-has-output-p (command)
  "Returns t if COMMAND will print meaningful information."
  (not (string= "nohup " (substring command 0 6))))

;; Redefines this function from dired-aux to
;; - have the COMMAND in the output buffer name
;; - avoid opening a window when the command is not going to produce
;;   any output
(defun dired-run-shell-command (command)
  (let ((output-buffer (generate-new-buffer-name
                        (concat "*Shell Command Output: '" command "'*")))
        (handler
         (find-file-name-handler (directory-file-name default-directory)
                                 'shell-command)))
    (if handler
        (apply handler 'shell-command (list command))
      (if (dc:command-has-output-p command)
          (shell-command command output-buffer)
        (save-window-excursion
          (shell-command command output-buffer)))))
  nil)

