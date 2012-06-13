; Configuring dired

(eval-after-load "dired"
  '(progn
     (require 'dired-aux)
     (require 'dired-x)))

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
	    '(("\\.image" "~/Documents/configuration/scripts/pharo.sh")
	      ("\\.pdf" "acroread")
	      ("\\.sh" "bash")
	      ("\\.avi" "vlc")
	      ("\\.flv" "vlc")
	      ("\\.jar" "java -jar")
	      ("\\.pptx" "libreoffice")
	      ("\\.xls" "libreoffice")
	      ("\\.odt" "libreoffice")
	      ("\\.docx" "libreoffice")
	      ("\\.xlsx" "libreoffice")
	      ("\\.html" "firefox"))))
       (mapcar (lambda (pair)
		 (add-to-list 'dired-guess-shell-alist-user pair))
	       dired-guessing))))

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
