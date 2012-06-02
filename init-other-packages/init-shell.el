(require 'shell)

(defun my:shell-buffer-name (str)
  (format "* Shell: %s *" str))

(defun my:shell-buffer-name-from-dir ()
  "Returns a name for the current shell buffer.
This name is based on the currently visited directory."
  (let* ((home (expand-file-name (concat comint-file-name-prefix "~/")))
	 (homelen (length home))
	 (dir default-directory))
    ;; Replaces '/home/you/' by '~/'
    (and (>= (length dir) homelen)
	 (string= home (substring dir 0 homelen))
	 (setq dir (concat "~/" (substring dir homelen))))
    (my:shell-buffer-name dir)))

(defadvice shell-dirstack-message (after my:shell-dirstack-message activate)
  "Updates the shell buffer name when directory changes"
  (rename-buffer (my:shell-buffer-name-from-dir) t))

;; adapted from git://github.com/renard/emacs-el.git with a
;; dedicated shell per directory
(defun my:shell:run (&optional name)
  "Run shell in `default-directory' and set buffer name."
  (interactive)
  (shell (if name 
	     (my:shell-buffer-name name)
	   (my:shell-buffer-name-from-dir))))

(defun my:shell-filter-buffer-list ()
  "From the list of all buffers to switch to provided in
`ido-make-buffer-list-hook', only take those have a running shell inside"
  (setq ido-temp-list
	(delq nil
	      (mapcar 
	       (lambda (buf)
		 (let ((buffer (get-buffer buf)))
		   ;;Necessary because of `ido-use-virtual-buffers'
		   (when buffer
		     (with-current-buffer buffer
		       (and 
			(eq major-mode 'shell-mode)
			(comint-check-proc (current-buffer))
			buf)))))
	       ido-temp-list))))

(defun my:shell-switch-buffer ()
  "Like `ido-switch-buffer' but only presents buffers with a
running shell"
  (interactive)
  (add-hook 'ido-make-buffer-list-hook 'my:shell-filter-buffer-list nil t)
  (unwind-protect
      (shell (ido-switch-buffer))
    (remove-hook 'ido-make-buffer-list-hook 'my:shell-filter-buffer-list t)))

(let ((shell-launcher (kbd "C-M-'")))
  (global-set-key shell-launcher 'my:shell-switch-buffer)
  ;; (global-set-key shell-launcher 'my:shell:run)
  ;; (define-key shell-mode-map shell-launcher
  ;; 	 (lambda ()
  ;; 	   (interactive)
  ;; 	   (my:shell:run (read-string "Name for new shell? "))))
  )

