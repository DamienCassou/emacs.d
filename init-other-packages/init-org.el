(setq org-directory (expand-file-name "~/Documents/configuration/org"))
(setq org-agenda-files
      '("~/Documents/configuration/org/tasks.org"
        "~/Documents/configuration/org/someday.org"
        "~/Documents/configuration/org/repeating.org"))

(setq org-refile-targets `(("tasks.org"      :maxlevel . 2)
			   ("someday.org"    :maxlevel . 2)
                           ("repeating.org"  :level    . 1)))

(setq org-file-apps
      '((auto-mode . emacs) ;; by default, open with emacs
	("\\.x?html?\\'" . default)
	("pdf" . "/usr/bin/acroread %s")
	(t . "/usr/bin/gnome-open %s")))

(setq org-agenda-default-appointment-duration 60)
(setq org-agenda-restore-windows-after-quit t)
(setq org-clock-clocked-in-display nil)
(setq org-completion-use-ido t)
(setq org-default-notes-file "tasks.org")
(setq org-fontify-done-headline t)
(setq org-hide-leading-stars t)
(setq org-imenu-depth 1)
(setq org-log-done 'time)
(setq org-mobile-directory "~/Dropbox/MobileOrg/")
(setq org-mobile-inbox-for-pull "~/Documents/configuration/org/from-mobile.org")
(setq org-special-ctrl-a/e t)
(setq org-time-stamp-rounding-minutes '(10 10))

(setq org-todo-keywords
      '((sequence "TODO(t)"    "|" "DONE(d)" "CANCELLED(c)")
        (sequence "APPT(p)"    "|" "DONE(d)" "CANCELED(c)")
        (sequence "WAITING(w)" "|" "DONE(d)")))

(setq org-todo-keyword-faces
      '(("NEXT" :foreground "orange" :weight bold)
        ("CANCELLED" :foreground "forest green")))

(setq org-modules '(org-bbdb org-bibtex org-docview org-gnus
                             org-habit org-info org-jsinfo org-habit
                             org-irc org-mew org-mhe org-protocol org-rmail org-vm
                             org-wl org-w3m org-bookmark org-pomodoro
                             org-pomodoro-pidgin org-publish))

;;;###autoload
(define-key global-map (kbd "C-. o t") 'org-capture)
;;;###autoload
(define-key global-map (kbd "C-. o a") 'org-agenda)
(define-key global-map (kbd "C-. o ,") 'org-cycle-agenda-files)

(setq org-capture-templates '())
(add-to-list
 'org-capture-templates
 '("t" "Todo [inbox]" entry (file+headline org-default-notes-file "Tasks")
   "* TODO %?%i\n %a"))

;; Display the agenda
(defun nico/jump-to-org-agenda ()
  (interactive)
  (let ((buffer (get-buffer "*Org Agenda*")))
    (if buffer
	(switch-to-buffer buffer)
      (org-agenda-list))
    (delete-other-windows)))

;; Go to the agenda buffer after 10' idle
(run-with-idle-timer 600 t 'nico/jump-to-org-agenda)

(defvar-local dc:org-publish-on-save nil
  "Set to t if you want to publish the project on each save.")
(defun dc:org-publish-on-save ()
  "Publish the current project."
  (when dc:org-publish-on-save
    (save-excursion
      (org-publish-current-project))))

(defun org-publish-lesscss (plist filename pub-dir)
  "Publish a file with no transformation of any kind.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (save-excursion
    (unless (file-directory-p pub-dir)
      (make-directory pub-dir t))
    (unless (equal (expand-file-name (file-name-directory filename))
                   (file-name-as-directory (expand-file-name pub-dir)))
      (let* ((cssfilename (format "%s.css" (file-name-base filename)))
             (destination (expand-file-name cssfilename pub-dir))
             ret output)
        (message "Compiling %s to %s" filename destination)
        ;; lessc lessfilename destination
        (with-temp-buffer
          (setq ret (call-process-shell-command "lessc" nil t nil
                                                filename
                                                destination))
          (setq output (buffer-string)))
        (unless (= ret 0)
          (message "Can't compile less file %s. %s" filename output))))))

(defmath vwsum (vec1 vec2)
  (+ (* vec1 vec2)))

(add-hook 'after-save-hook #'dc:org-publish-on-save)
