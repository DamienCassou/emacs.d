; Configuring dired

(eval-after-load "dired"
  '(progn
     (require 'dired-aux)
     (require 'dired-x)
     (defun dired-do-ispell (&optional arg)
       "Mark files in dired before running this function and they will
all get spell checked."
       (interactive "P")
       (dolist (file (dired-get-marked-files
		      nil arg
		      #'(lambda (f)
			  (not (file-directory-p f)))))
	 (save-window-excursion
	   (with-current-buffer (find-file file)
	     (ispell-buffer)))
	 (message nil)))

     (eval-after-load "recentf"
       '(progn
	  (defun recentf-track-dired-buffers ()
	    "I want the dired buffers to be tracked by recentf"
	    (let ((cur-dir-no-slash (substring ; removes trailing slash
				     (expand-file-name default-directory)
				     0 -1)))
	      (when (and (file-directory-p cur-dir-no-slash)
			 (not (string-equal "/" (substring cur-dir-no-slash -1))))
		(recentf-add-file cur-dir-no-slash))))
	  (add-hook 'dired-mode-hook 'recentf-track-dired-buffers t)))))

(eval-after-load "dired-aux"
  '(progn
     ;; Redefines this function from dired-aux to pass a buffer name
     ;; as parameter to shell-command. Because of this, we can now
     ;; execute multiple asynchronous commands, each with its own
     ;; buffer
     (defun dired-run-shell-command (command)
       (let ((handler
	      (find-file-name-handler (directory-file-name default-directory)
				      'shell-command)))
	 (if handler (apply handler 'shell-command (list command))
	   (shell-command command
			  ;; Damien: only the following sexp is changed:
			  (generate-new-buffer-name
			   (concat "*Shell Command Output: '" command "'*")))))
       ;; Return nil for sake of nconc in dired-bunch-files.
       nil)
     (add-to-list
      'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip"))))

(eval-after-load "dired-x"
  '(progn
     (let ((extensions-to-ignore '(".out" ".lol")))
       (mapcar (lambda (extension)
		 (add-to-list 'completion-ignored-extensions extension)
		 (add-to-list 'dired-omit-extensions extension))
	       extensions-to-ignore))
     (let ((files-to-ignore '("Thumbs.db" "Thumbs.db:encryptable")))
       (mapcar (lambda (filename)
		 (setq dired-omit-files
		       (concat dired-omit-files "\\|^" filename "$")))
	       files-to-ignore))
     (add-hook 'dired-mode-hook
      (lambda () (dired-omit-mode)))
     (let ((dired-guessing
	    '(
	      ("\\.avi" "vlc")
	      ("\\.doc" "libreoffice")
	      ("\\.docx" "libreoffice")
	      ("\\.flv" "vlc")
	      ("\\.html" "firefox")
	      ("\\.image" "~/Documents/configuration/scripts/pharo.sh")
	      ("\\.jar" "java -jar")
	      ("\\.mkv" "vlc")
	      ("\\.mp4" "vlc")
	      ("\\.odt" "libreoffice")
	      ("\\.ogv" "vlc")
	      ("\\.pdf" "acroread")
	      ("\\.pptx" "libreoffice")
	      ("\\.sh" "bash")
	      ("\\.svg" "inkscape")
	      ("\\.webm" "vlc")
	      ("\\.xls" "libreoffice")
	      ("\\.xlsx" "libreoffice")
)))
       (mapcar (lambda (pair)
		 (add-to-list 'dired-guess-shell-alist-user pair))
	       dired-guessing))))
