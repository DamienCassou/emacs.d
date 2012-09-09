(defconst my:prompt-defaults
  `(("username" . (user-login-name))
    ("hostname" . (substring (shell-command-to-string "hostname") 0 -1))))

(defconst my:prompt-default-values
  (mapcar (lambda (pair) (cons (car pair) (eval (cdr pair))))
	  my:prompt-defaults))

(defun my:prompt-value (variable)
  "Returns current value for VARIABLE if it's not default, "" otherwise."
  (let ((default (cdr (assoc variable my:prompt-default-values)))
	(current (eval (cdr (assoc variable my:prompt-defaults)))))
    (if (string= default current)
	""
      current)))

(defvar my:prompt-inserted nil)

(defun my:color (type)
  (case type
    ('alert "red")
    ('good "lime green")
    ('warning "dark orange")
    (t "black")))

(defun my:mprint (obj &optional color)
  (if color
      (insert (propertize obj 'face `(:foreground ,(my:color color))))
    (insert obj)))

(defun my:insert-value (value &optional color)
  (my:mprint value color)
  (setq my:prompt-inserted (not (string= value ""))))

(defun my:insert-variable (variable)
  (my:insert-value (my:prompt-value variable)))

(defun my:insert-separator (&optional separator)
  (let ((sep (or separator ":")))
    (if my:prompt-inserted
	(my:mprint sep))))

(defun my:insert-pwd ()
  (let ((pwd (abbreviate-file-name (eshell/pwd))))
    (my:insert-value pwd)))

(defun my:git-color ()
  "Returns a color code based on the current repository status"
  (if (zerop (magit-git-exit-code "diff" "--quiet"))
      ;; nothing to commit because nothing changed
      (if (zerop (length (magit-git-string
			  "rev-list" (concat "origin/"
					     (magit-get-current-branch)
					     ".."
					     (magit-get-current-branch)))))
	  ;; nothing to push as well
	  'good
	;; nothing to commit, but some commits must be pushed
	'warning)
    'alert))

(defun my:insert-branch ()
  (let ((branch (magit-get-current-branch)))
    (if branch
	(my:mprint (concat " <" branch ">") (my:git-color)))))

(defun my:eshell-prompt ()
  (with-temp-buffer
    (my:insert-variable "username")
    (my:insert-separator "@")
    (my:insert-variable "hostname")
    (my:insert-separator ":")
    (my:insert-pwd)
    (my:insert-branch)
    (my:insert-value (if (= (user-uid) 0) " # " " $ "))
    (buffer-substring (point-min) (point-max))))

(eval-after-load "em-prompt"
  '(progn
     (setq eshell-highlight-prompt nil)
     (setq eshell-prompt-function 'my:eshell-prompt)))

(eval-after-load "em-term"
  '(progn
     (add-to-list 'eshell-visual-commands "htop")))
