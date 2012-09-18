(setq org-directory (expand-file-name "~/Documents/configuration/org"))
(setq org-agenda-files (list org-directory))

(setq org-file-apps
      '((auto-mode . emacs) ;; by default, open with emacs
	("\\.x?html?\\'" . default)
	("pdf" . "/usr/bin/acroread %s")
	(t . "/usr/bin/gnome-open %s")))

(setq org-hide-leading-stars t)
(setq org-log-done 'time)
(setq org-special-ctrl-a/e t)
(setq org-time-stamp-rounding-minutes '(10 10))
(setq org-default-notes-file "tasks.org")

;;;###autoload
(define-key global-map (kbd "s-c") 'org-capture)
;;;###autoload
(define-key global-map (kbd "C-,") 'org-cycle-agenda-files)

(setq org-capture-templates '())

(add-to-list 'org-capture-templates
	     `("c" "Cadeaux" entry
	       (file "cadeaux.org")
	       "* %i%?\n%^g"))

(add-to-list 'org-capture-templates
	     `("t" "Tâches" entry
	       (file+headline "tasks.org" "Unsorted")
	       "* TODO %i%?\n"))
