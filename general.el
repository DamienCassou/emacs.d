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
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/zenburn-emacs")
(load-theme 'zenburn t nil)

;; Clipboard handling
(global-set-key (kbd "C-w") 'clipboard-kill-region)
(global-set-key (kbd "M-w") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-y") 'clipboard-yank)

; Configuring dired

(eval-after-load "dired-aux"
  '(add-to-list
    'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip")))

(require 'dired)
(load "dired-x")

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

;; Allows for S-left, S-top... to change selected buffer frame
(windmove-default-keybindings)

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; Provides a keyboard way to find recent files
(require 'recentf)
(defun xsteve-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read "Recentf open: "
                          (mapcar (lambda (path)
                                    (replace-regexp-in-string home "~" path))
                                  recentf-list)
                          nil t))))
(global-set-key (kbd "C-c r") 'xsteve-ido-choose-from-recentf)

(defun LaTeX-align-table ()
  (interactive)
  (save-excursion
    (LaTeX-mark-environment)
    (while (re-search-forward "& *" (region-end) t)
      (replace-match "& " nil nil))
    (LaTeX-mark-environment)
    (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)\\(&\\|\\\\\\\\\\)" 1 1 t)))

(defun ido-execute ()
  "It would be really nice if ido mode could be implemented also
for M-x (command completion)."
  (interactive)
  (call-interactively
   (intern
    (ido-completing-read
     "M-x "
     (let (cmd-list)
       (mapatoms (lambda (S) (when (commandp S) (setq cmd-list (cons (format "%S" S) cmd-list)))))
       cmd-list)))))

(global-set-key (kbd "M-x") 'ido-execute)

(global-set-key (kbd "<f2>") 'indent-region)
(global-set-key (kbd "<f5>") 'comment-region)

(defun my-tab-fix ()
  (interactive)
  (local-set-key [tab] 'indent-according-to-mode))

(global-set-key (kbd "s-SPC")          'hippie-expand)

(add-hook 'LaTeX-mode-hook      'my-tab-fix)
(add-hook 'sh-mode-hook         'my-tab-fix)
(add-hook 'emacs-lisp-mode-hook 'my-tab-fix)
(add-hook 'lisp-mode-hook       'my-tab-fix)

;; Byte compile all emacs list files in ~/.emacs.d
(defun byte-compile-my-emacs-files ()
  (interactive)
  (dolist (file (directory-files "~/.emacs.d/" t "\.el$"))
    (unless (byte-compile-file file)
      (error "Byte compile failed for: %s" file))))


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

(defun find-alternative-file-with-sudo ()
  (interactive)
  (let ((fname (or buffer-file-name
		   dired-directory)))
    (when fname
      (if (string-match "^/sudo:root@localhost:" fname)
	  (setq fname (replace-regexp-in-string
		       "^/sudo:root@localhost:" ""
		       fname))
	(setq fname (concat "/sudo:root@localhost:" fname)))
      (find-alternate-file fname))))


;; ediff
(require 'ediff)

;; misc functions
(load "functions")

;; Java
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-eclim/"))
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-eclim/vendor"))
;; (require 'eclim)

;; (setq eclim-auto-save t)
;; (global-eclim-mode)
 
