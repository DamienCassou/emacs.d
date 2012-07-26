(defun suspend-on-tty-only ()
  (interactive)
  (unless window-system
    (suspend-frame)))

(when window-system
  (global-set-key (kbd "C-z") 'suspend-on-tty-only)
  (global-set-key (kbd "C-x C-z") 'suspend-on-tty-only))

;; Clipboard handling
(global-set-key (kbd "C-w") 'clipboard-kill-region)
(global-set-key (kbd "M-w") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-y") 'clipboard-yank)

;; Sets the frame title:
(setq frame-title-format '("Emacs: " (buffer-file-name " %f")))

;; Orgmode
;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "<f2>") 'indent-region)
(global-set-key (kbd "<f5>") 'comment-region)

(global-set-key (kbd "s-SPC")          'hippie-expand)

;; Press C-o when in search mode and all occurrences will appear
;; http://www.emacsblog.org/2007/02/27/quick-tip-add-occur-to-isearch/
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

;; misc functions
(load "functions")

;; Prevents existing by accident
(global-set-key (kbd "C-x C-c")
		(lambda (arg)
		  (interactive "P")
		  (if arg
		      (save-buffers-kill-terminal)
		    (message "I don't want to quit. If you really want to, use prefix arg `C-u'"))))

;; Don't know why "C-h i" opens info in current window whereas the
;; others open in other window (like "C-h f")
(global-set-key (kbd "C-h i") 'info-other-window)

;; Removes whitespace at the end of lines
(add-hook 'write-file-functions 'delete-trailing-whitespace)

;; Each file in init-other-packages named init-<somelibrary>.el is
;; loaded just after its corresponding library.
(let ((other-pkgs-dir "~/.emacs.d/init-other-packages"))
  (add-to-list 'load-path other-pkgs-dir)

  (dolist (file (directory-files other-pkgs-dir) t)
    (when (string-match (format "^init-\\(.+\\)\\.el$") file)
      (eval-after-load (match-string-no-properties 1 file)
	`(load ,file)))))
