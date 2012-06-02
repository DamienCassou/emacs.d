(require 'cl)
(require 'tramp)

(defvar buffers-to-keep '("*scratch*" "*Messages*"))
(defun buffer-killable-p (buffer)
  (and
   (not (member (buffer-name buffer) buffers-to-keep))
   (or (null (buffer-file-name buffer)) ;; buffer is not a file
       (not (buffer-modified-p buffer))))) ;; or file is not modified

(defun kill-all-buffers ()
  (interactive)
  (let ((count 0))
    (dolist (buffer (buffer-list))
      (message "Count is %s" count)
      (when (buffer-killable-p buffer)
	(incf count)
	(message "Killing %s" (buffer-name buffer))
	(kill-buffer buffer)))
      (message "%s buffers have been killed" count)))



;; Local Variables:
;; lexical-binding: t
;; End:
