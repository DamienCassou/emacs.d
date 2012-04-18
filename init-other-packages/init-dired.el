; Configuring dired

(eval-after-load "dired-aux"
  '(progn
     ;; Redefines this function from dired-aux to pass a buffer name
     ;; as parameter to shell-command
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

(require 'dired)
(load "dired-x")

(add-to-list 'completion-ignored-extensions ".log")
(add-to-list 'dired-omit-extensions ".log")
(add-to-list 'completion-ignored-extensions ".out")
(add-to-list 'dired-omit-extensions ".out")
(add-to-list 'dired-omit-extensions ".lol")

(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))

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
