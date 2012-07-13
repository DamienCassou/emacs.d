(defconst my:prompt-defaults
  `(("username" . (user-login-name))
    ("hostname" . (substring (shell-command-to-string "hostname") 0 -1))))

(defconst my:prompt-default-values
  (mapcar (lambda (pair) (cons (car pair) (eval (cdr pair))))
	  my:prompt-defaults))

(defun prompt-value (variable)
  "Returns current value for VARIABLE if it's not default, "" otherwise."
  (let ((default (cdr (assoc variable my:prompt-default-values)))
	(current (eval (cdr (assoc variable my:prompt-defaults)))))
    (if (string= default current)
	""
      current)))

(defvar prompt-inserted nil)

(defun mprint (obj)
  (princ obj))

(defun insert-value (value)
  (mprint value)
  (setq prompt-inserted (not (string= value ""))))

(defun insert-variable (variable)
  (insert-value (prompt-value variable)))

(defun insert-separator (&optional separator)
  (let ((sep (or separator ":")))
    (if prompt-inserted
	(mprint sep))))

(defun insert-pwd ()
  (let ((pwd (abbreviate-file-name (eshell/pwd))))
    (insert-value pwd)))

(defun insert-branch ()
  (let ((branch (magit-get-current-branch)))
    (mprint (if branch
		(concat " <" (magit-get-current-branch) ">")
	      ""))))

(defun my:eshell-prompt ()
  (with-output-to-string
    (insert-variable "username")
    (insert-separator "@")
    (insert-variable "hostname")
    (insert-separator ":")
    (insert-pwd)
    (insert-branch)
    (if (= (user-uid) 0)
	(insert-value "# ")
      (insert-value "$ "))))

(setq eshell-prompt-function 'my:eshell-prompt)
