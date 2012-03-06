(require 'cl)
(require 'tramp)

"from: git://github.com/renard/emacs-el.git"
(defmacro cw:with-parse-directory (sudo &rest body)
  "Execute BODY with a declaration of following variables after
setting `default-directory' to the directory of file file visited
in current buffer.

If SUDO is not nil `method' is set to \"sudo\" and `user' to
\"root\".
"
  `(let* ((sudo ,sudo)
	  (current-buffer-dir
	   ;; get directory name from either dired or buffer file name and
	   ;; fall back to nil
	   (or (ignore-errors (dired-current-directory))
	       (ignore-errors (file-name-directory (buffer-file-name)))
	       "~"))
	  (file-vector
	   ;; get a tramp usable file URI from directory.
	   (or (ignore-errors (tramp-dissect-file-name current-buffer-dir))
	       (tramp-dissect-file-name (concat "/:" current-buffer-dir) 1)))
	  ;; split file URI into its components
	  (method (if ,sudo "sudo" (tramp-file-name-method file-vector)))
	  (user (if ,sudo "root" (tramp-file-name-user file-vector)))
	  (localname (tramp-file-name-localname file-vector))
	  (host (tramp-file-name-host file-vector))
	  (default-directory
	    ;; If no method is defined then the file is local
	    ;; then don't use tramp.
	    (if method
		(tramp-make-tramp-file-name method user host localname)
	      localname)))
     ,@body))

;; adapted from git://github.com/renard/emacs-el.git with a dedicated
;; shell per directory
(defun cw:shell:run ()
  "Run shell in `default-directory' and set buffer name."
  (interactive)
  (shell (format "* Shell: %s *" default-directory)))

(global-set-key (kbd "C-M-'") 'cw:shell:run)

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
