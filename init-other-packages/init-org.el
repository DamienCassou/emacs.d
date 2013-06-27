(add-to-list 'org-modules 'habit)
(require 'org-protocol)

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
(define-key global-map (kbd "C-. o t") 'org-capture)
;;;###autoload
(define-key global-map (kbd "C-,") 'org-cycle-agenda-files)
;;;###autoload
(define-key global-map (kbd "C-. o a") 'org-agenda)

(setq org-capture-templates '())

(add-to-list 'org-capture-templates
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
