(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(define-key ctl-x-4-map "t" 'toggle-window-split)

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
