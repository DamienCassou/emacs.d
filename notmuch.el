(add-to-list 'load-path "~/usr/share/emacs/site-lisp")
(require 'json)
(require 'notmuch)

(defvar notmuch-gmail-base-directory "/home/cassou/Mail/GmailTest")
(defvar notmuch-gmail-hide-folders '("[Gmail].All Mail"))

(defun notmuch-gmail-add-to-folder (folder)
  "Copy the current mail to IMAP FOLDER."
  (interactive
   (list (completing-read "Copy to which folder? "
			  (notmuch-gmail-all-possible-target-folders)
			  nil ;; predicate
			  t ;; require match?
			  )))
  (if (or (zerop (length folder)) (null folder))
      (error "Target folder can not be empty"))
  (if (member folder (notmuch-gmail-mail-folders))
      (error "Message already in target folder %s" folder))
  (let ((target-folder (expand-file-name (concat folder "/cur")
					 notmuch-gmail-base-directory)))
    (unless (file-directory-p target-folder)
      (error "Target folder is not an existing directory: %s" target-folder))
    (copy-file (notmuch-gmail-file) target-folder))
  (notmuch-gmail-refresh))

(defun notmuch-gmail-remove-from-folder (folder)
  "Remove the current mail from IMAP FOLDER."
  (interactive
   (list (completing-read "Remove from which folder? "
			  (notmuch-gmail-mail-folders)
			  nil ;; predicate
			  t))) ;; require match?
  (if (or (zerop (length folder)) (null folder))
      (error "Target folder can not be empty"))
  (if (not (member folder (notmuch-gmail-mail-folders)))
      (error "Message is not in folder %s" folder))
  (let ((file (car
	       (member-if
		(lambda (f)
		  (string-match
		   (concat "^" notmuch-gmail-base-directory "/"
			   (regexp-quote folder) "/")
		   f))
		(notmuch-gmail-files)))))
    (delete-file file))
  (notmuch-gmail-refresh))

(defun notmuch-gmail-refresh ()
  "Ensure that notmuch database knows about the latest changes."
  (notmuch-poll))

(defun notmuch-gmail-all-possible-target-folders ()
  (set-difference
   (notmuch-gmail-all-folders)
   (notmuch-gmail-mail-folders)
   :test #'string-equal))

(defun notmuch-gmail-all-folders ()
  "Return the list of all IMAP folders."
  (set-difference
   (directory-files notmuch-gmail-base-directory)
   '("." ".." ".notmuch")
   :test #'string-equal))

(defun notmuch-gmail-file ()
  "Return one file paths containing the current mail."
  (notmuch-show-get-filename))

(defun notmuch-gmail-get-message-id ()
  (case major-mode
    (notmuch-show-mode (notmuch-show-get-message-id))
    (otherwise (error "Don't know how to get current message id"))))

(defun notmuch-gmail-files ()
  "Return the file paths containing the current mail."
  (save-window-excursion
    (notmuch-gmail-search-files (notmuch-gmail-get-message-id))
    (goto-char (point-min))
    (let* ((json-array-type 'list)
	   (files (json-read)))
      (kill-buffer)
      files)))

(defun notmuch-gmail-mail-folder (file)
  "Return the IMAP folder containing FILE."
  (let ((path (file-relative-name file notmuch-gmail-base-directory)))
    (substring path 0 (string-match "/" path))))

(defun notmuch-gmail-mail-folders (&optional files)
  "Return the IMAP folders containing FILES.
If FILES is nil, return the folders for the current files."
  (mapcar 'notmuch-gmail-mail-folder
	  (or files
	      (notmuch-gmail-files))))

(defun notmuch-gmail-search-files (query)
  (let ((buffer (get-buffer-create (notmuch-search-buffer-title query))))
    (switch-to-buffer buffer)
    ;; Don't track undo information for this buffer
    (set 'buffer-undo-list t)
    (erase-buffer)
    (goto-char (point-min))
    (call-process
     notmuch-command
     nil ;; input
     buffer  ;; output
     nil ;; show progress
     "search"
     "--format=json"
     "--output=files"
     query)))

(defun notmuch-gmail-show-folders ()
  "Show the folders the current thread is in."
  (let* ((mail-folders (set-difference
			(notmuch-gmail-mail-folders)
			notmuch-gmail-hide-folders)))
    (setq header-line-format
	  (concat header-line-format
		  (format " %s" mail-folders)))))

(add-hook 'notmuch-show-hook 'notmuch-gmail-show-folders t)
(remove-hook  'notmuch-show-hook 'notmuch-gmail-show-folders)

(define-key notmuch-show-mode-map (kbd "+") 'notmuch-gmail-add-to-folder)
(define-key notmuch-show-mode-map (kbd "-") 'notmuch-gmail-remove-from-folder)
