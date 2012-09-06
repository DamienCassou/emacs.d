(defun my:update-autoloads ()
  "Manually call this command when you want to update autoloads"
  (interactive)
  (let ((generated-autoload-file "~/.emacs.d/my-autoloads.el"))
    (if (file-exists-p generated-autoload-file)
	(delete-file generated-autoload-file t))
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

(defvar photo-themes "~/misc/Photos/evenement/2012_notre_mariage/par_theme")
(defvar link-image-history '())
(defun link-image (source destination-name)
  (interactive
   (list
    (dired-filename-at-point)
    (completing-read "Destination? "
		     (directory-files
		      photo-themes
		      nil
		      "^[^\\.]")
		     nil
		     'confirm-after-completion
		     nil
		     'link-image-history
		     (car link-image-history))))
  (unless (and source (file-exists-p source))
    (error "Invalid source: %s" source))
  (when (null destination-name)
      (error "Empty destination not allowed"))
  (let ((destination
	 (expand-file-name (concat photo-themes "/" destination-name))))
    (message "Destination = %s" destination)
    (when (not (file-directory-p destination))
      (make-directory destination))
    (message "%s"
	     (with-temp-buffer
	       (call-process "ln" nil (current-buffer) nil
			     source
			     (concat destination "/"))
	       (buffer-substring (point-min) (point-max))))))
(defun link-images ()
  (interactive)
  (while t
    (image-dired-dired-display-image)
    (call-interactively 'link-image)
    (dired-next-line 1)))
(define-key dired-mode-map (kbd ";") 'link-images)

;; Local Variables:
;; lexical-binding: t
;; End:
