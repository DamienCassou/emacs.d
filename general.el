;; Path
(add-to-list 'exec-path "~/Documents/configuration/scripts/")

(defun suspend-on-tty-only ()
  (interactive)
  (unless window-system
    (suspend-frame)))

(when window-system
  (global-set-key (kbd "C-z") 'suspend-on-tty-only)
  (global-set-key (kbd "C-x C-z") 'suspend-on-tty-only))

;; Change theme
(let ((themes-directory "~/.emacs.d/themes/"))
  (when (and window-system
	     (listp custom-theme-load-path)
	     (file-directory-p themes-directory))
    (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/zenburn-emacs")
    (load-theme 'zenburn t nil)))

;; Clipboard handling
(global-set-key (kbd "C-w") 'clipboard-kill-region)
(global-set-key (kbd "M-w") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-y") 'clipboard-yank)

;; Sets the frame title:
(setq frame-title-format '("Emacs: " (buffer-file-name " %f")))

;; Orgmode
;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(add-hook 'text-mode-hook 'flyspell-mode)

;; ispell must ignore LaTeX commands and environments
(setq ispell-tex-skip-alists
      (list
       (append (car ispell-tex-skip-alists)
               '(("\\\\cite"            ispell-tex-arg-end)
                 ("\\\\nocite"          ispell-tex-arg-end)
                 ("\\\\includegraphics" ispell-tex-arg-end)
                 ("\\\\figScale"         ispell-tex-arg-end)
                 ("\\\\author"          ispell-tex-arg-end)
                 ("\\\\ref"             ispell-tex-arg-end)
                 ("\\\\eqref"             ispell-tex-arg-end)
                 ("\\\\pageref"             ispell-tex-arg-end)
                 ("\\\\label"           ispell-tex-arg-end)
		 ("\\\\lstinputlisting" ispell-tex-arg-end)
		 ("\\\\enword" ispell-tex-arg-end)
		 ("\\\\ct" ispell-tex-arg-end)
		 ("\\\\sigle" ispell-tex-arg-end)
		 ("\\\\nocheck" ispell-tex-arg-end)
		 ("\\\\mathit" ispell-tex-arg-end)
		 ("\\\\url" ispell-tex-arg-end)
		 ("\\\\lst[p]?\\(lignesa\\|lignes\\|ligne\\)" ispell-tex-arg-end 2)
		 ("\\\\\\(lignesa\\|lignes\\|ligne\\)" ispell-tex-arg-end)
		 ("\\\\c?cauthor" ispell-tex-arg-end)
		 ("\\\\page" ispell-tex-arg-end)
		 ("\\\\listing" ispell-tex-arg-end)
		 ("\\\\macitationraw" ispell-tex-arg-end 2)
		 ("\\\\macitation" ispell-tex-arg-end 3)
		 ("\\\\mm" ispell-tex-arg-end)
		 ("\\\\begin{lstlisting}" . "\\\\end{lstlisting}")
		 ("\\\\begin{code}{}" . "\\\\end{code}")
                 ))
       (append (cadr ispell-tex-skip-alists)
	       '(("tabular" ispell-tex-arg-end)
		 ("equation\\*" . "\\\\end[ 	\n]*{[ 	\n]*equation\\*[ 	\n]*}")
		 ("tikzpicture" . "\\\\end[ 	\n]*{[ 	\n]*tikzpicture[ 	\n]*}")))))

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

(defun my-mark-word ()
  "Marks the whole word at point contrary to builtin mark-word which only marks the end"
  (interactive)
  (let ((word-pos (bounds-of-thing-at-point 'word)))
    (goto-char (car word-pos))
    (set-mark (point))
    (goto-char (cdr word-pos))))
(global-set-key (kbd "M-@") 'my-mark-word)

;; misc functions
(load "functions")

;; Prevents existing by accident
(global-set-key (kbd "C-x C-c")
		(lambda (arg)
		  (interactive "P")
		  (if arg
		      (save-buffers-kill-terminal)
		    (message "I don't want to quit. If you really want to, use prefix arg `C-u'"))))

(add-to-list 'load-path "~/.emacs.d/init-other-packages")
(load "init-which-func")
(load "init-ediff")
(load "init-dired")
(load "init-bibtex")
