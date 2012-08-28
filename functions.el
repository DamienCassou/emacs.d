(defun my:update-autoloads ()
  "Manually call this command when you want to update autoloads"
  (interactive)
  (let ((generated-autoload-file "~/.emacs.d/my-autoloads.el"))
    (delete-file generated-autoload-file t)
    (update-directory-autoloads "~/.emacs.d")
    (update-directory-autoloads  "~/.emacs.d/init-other-packages")
    (load generated-autoload-file)
    (let ((buf (get-file-buffer generated-autoload-file)))
      (when buf (kill-buffer buf)))))

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
      (when (buffer-killable-p buffer)
	(incf count)
	(message "Killing %s" (buffer-name buffer))
	(kill-buffer buffer)))
      (message "%s buffers have been killed" count)))



;; Local Variables:
;; lexical-binding: t
;; End:
