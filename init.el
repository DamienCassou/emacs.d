;;; init.el --- user-init-file                    -*- lexical-binding: t -*-


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(setq package-enable-at-startup nil)

(progn ;    `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require  'borg)
  (borg-initialize))

(require 'diminish)

(progn ; `use-package'
  (setq use-package-always-defer t)
  (setq use-package-enable-imenu-support t)
  (setq use-package-minimum-reported-time 0)
  (setq use-package-verbose t)
  (require 'use-package))

(use-package no-littering
  :demand t)

(progn ; `startup'
  (setq inhibit-startup-screen t)
  (setq initial-buffer-choice t)
  (setq initial-major-mode 'text-mode)
  (setq initial-scratch-message nil)
  (setq user-mail-address "damien@cassou.me"))

(progn ; `files'
  (setq confirm-kill-emacs 'y-or-n-p)
  (setq make-backup-files nil)
  (setq version-control t))

(use-package simple
  :demand t
  :init
  (progn
    (setq delete-active-region nil)
    (setq eval-expression-print-length 20)
    (setq eval-expression-print-level nil)
    (setq save-interprogram-paste-before-kill t))
  :config
  (progn
    (column-number-mode)))

(use-package auto-compile
  :demand t
  :init
  (progn
    (setq auto-compile-display-buffer nil)
    (setq auto-compile-mode-line-counter t)
    (setq auto-compile-source-recreate-deletes-dest t)
    (setq auto-compile-toggle-deletes-nonlib-dest t)
    (setq auto-compile-update-autoloads t))
  :config
  (progn
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)
    (add-hook 'auto-compile-inhibit-compile-hook
              'auto-compile-inhibit-compile-detached-git-head)))

(use-package epkg
  :init
  (progn
    (setq epkg-repository
          (no-littering-expand-var-file-name "epkgs"))))

(use-package custom
  :demand t
  :init
  (progn
    (customize-set-variable
     'custom-safe-themes
     '("5a603291fd17c6e27fa16f644e23091ac40b3802c292e4e8fd6632f3f9c5d0de"
       default)))
  :config
  (progn
    (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
    (when (file-exists-p custom-file)
      (load custom-file))))

(use-package server
  :config
  (progn
    (unless (or (daemonp) (server-running-p))
      (server-start))))

(use-package font-core
  :demand t
  :config
  (progn
    (global-font-lock-mode)))

(use-package minibuffer
  :init
  (progn
    (setq read-file-name-completion-ignore-case t)))

(use-package elec-pair
  :demand t
  :config
  (progn
    (electric-pair-mode)))

(use-package prog-mode
  :demand t
  :config
  (global-prettify-symbols-mode))

(use-package saveplace
  :demand t
  :config
  (progn
    (save-place-mode)))

(use-package uniquify
  :init
  (progn
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets)))

(use-package tooltip
  :demand t
  :config
  (progn
    (tooltip-mode -1)))

(use-package winner
  :demand t
  :config
  (progn
    (winner-mode)))

(use-package nsm ;; network security
  :init
  (progn
    (setq nsm-save-host-names t)))

(use-package imenu
  :init
  (progn
    (setq imenu-auto-rescan t)
    (setq imenu-max-item-length 200)))

(use-package package
  :init
  (progn
    (setq package-archive-priorities '(("melpa-stable" . 10)))))

(use-package proced
  :init
  (progn
    (setq proced-filter 'all)))

(use-package emacsbug
  :init
  (progn
    (setq report-emacs-bug-no-explanations t)))

(use-package debbugs-gnu
  :init
  (progn
    (setq debbugs-gnu-trunk-directory "~/Documents/projects/emacs/emacs-src")))

(use-package smime
  :config
  ;; https://src.fedoraproject.org/rpms/emacs/blob/f27/f/default.el
  (setq smime-CA-directory "/etc/ssl/certs"))

(use-package undo-tree
  :demand t
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (define-key undo-tree-map (kbd "C-x r") nil)))

(use-package dired
  :bind (("C-x C-j" . dired-jump))
  :init
  (progn
    (setq dired-dwim-target t)
    (setq dired-listing-switches "-alh")
    (setq dired-recursive-deletes 'always))
  :config
  (progn
    (add-hook 'dired-mode-hook #'turn-on-gnus-dired-mode)

    (bind-key ")" 'dired-omit-mode dired-mode-map)

    (add-hook 'dired-mode-hook
              (lambda ()
                (dired-omit-mode)
                (dired-hide-details-mode 1)))

    (defun dired-move-beginning-of-line ()
      (interactive)
      (let ((point (point)))
        (dired-move-to-filename)
        (when (= point (point))
          (move-beginning-of-line nil))))

    (define-key dired-mode-map
      (vector 'remap 'move-beginning-of-line) 'dired-move-beginning-of-line)))

(use-package runner
  :after dired
  :init
  (progn
    (setq runner-run-in-background t)))

(use-package dired-x
  :after dired
  :init
  (progn
    (setq dired-omit-verbose nil)))

(use-package dired-imenu
  :after dired)

(use-package recentf
  :demand t
  :init
  (progn
    (setq recentf-auto-cleanup 300)
    (setq recentf-exclude '("~$" "\\.log$"))
    (setq recentf-max-saved-items 4000))
  :config
  (progn
    (defun recentf-track-dired-buffers ()
      "I want the dired buffers to be tracked by recentf"
      (let ((cur-dir-no-slash (substring ; removes trailing slash
                               (expand-file-name default-directory)
                               0 -1)))
        (when (and (file-directory-p cur-dir-no-slash)
                   ;; because of recentf limitations,
                   ;; - we can't store the filesystem root
                   (not (zerop (length cur-dir-no-slash)))
                   ;; - we can't store a TRAMP root
                   (not (string-equal ":" (substring cur-dir-no-slash -1)))
                   ;; And I prefer not storing TRAMP files
                   (or (not (featurep 'tramp))
                       (not (tramp-tramp-file-p cur-dir-no-slash))))
          ;; recentf does not play well with file ending with a slash
          (recentf-add-file cur-dir-no-slash))))
    (add-hook 'dired-mode-hook 'recentf-track-dired-buffers t)
    (recentf-mode)))

(use-package magit
  :diminish (magit-auto-revert-mode magit-wip-after-save-mode magit-wip-after-apply-mode magit-wip-affter-change)
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-dispatch-popup))
  :init
  (progn
    (setq magit-diff-refine-hunk 'all)
    (setq magit-process-find-password-functions '(magit-process-password-auth-source))
    (setq magit-wip-after-apply-mode nil)
    (setq magit-wip-after-save-mode nil)
    (setq magit-wip-before-change-mode nil)
    (setq magit-branch-prefer-remote-upstream '("master"))
    (setq magit-branch-adjust-remote-upstream-alist '(("origin/master" "master"))))
  :config
  (progn
    (global-magit-file-mode)
    ;; Enable magit-clean
    (put 'magit-clean 'disabled nil)
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-modules
                            'magit-insert-unpulled-from-upstream)))

(use-package vc-hooks
  :init
  (progn
    (setq vc-follow-symlinks nil)))

(use-package ediff-wind
  :init
  (progn
    (setq ediff-split-window-function 'split-window-horizontally)
    (setq ediff-window-setup-function 'ediff-setup-windows-plain)))

(use-package ace-link
  :demand t
  :config
  (progn
    (ace-link-setup-default)))

(use-package ispell
  :bind
  (("C-. d b" . ispell-buffer)
   ("C-. d f" . ispell-change-dictionary-to-french)
   ("C-. d e" . ispell-change-dictionary-to-english))
  :init
  (progn
    (setq ispell-dictionary "english")
    (setq ispell-program-name (executable-find "hunspell")))
  :config
  (progn
    (defun ispell-set-dictionary (dict)
      (save-excursion
        (add-file-local-variable 'ispell-local-dictionary dict)))

    (defun ispell-change-dictionary-to-french (arg)
      (interactive "P")
      (ispell-change-dictionary "francais")
      (when arg
        (ispell-set-dictionary "francais"))
      (flyspell-buffer))

    (defun ispell-change-dictionary-to-english (arg)
      (interactive "P")
      (ispell-change-dictionary "english")
      (when arg
        (ispell-set-dictionary "english"))
      (flyspell-buffer))))

(use-package flyspell
  :diminish flyspell-mode
  :bind (("C-. f b" . flyspell-buffer))
  :commands (flyspell-mode)
  :init
  (progn
    (setq flyspell-use-meta-tab nil)
    (add-hook 'text-mode-hook #'flyspell-mode))
  :config
  (progn
    (unbind-key "C-." flyspell-mode-map)))

(use-package eldoc
  :diminish eldoc-mode
  :commands (eldoc-mode)
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)))

(use-package checkdoc
  :diminish checkdoc-minor-mode
  :init
  (progn
    (setq checkdoc-spellcheck-documentation-flag t)))

(use-package face-remap
  :diminish text-scale-mode)

(use-package flycheck
  :diminish flycheck-mode
  :commands (flycheck-mode)
  :init
  (progn
    (add-hook 'prog-mode-hook #'flycheck-mode)))

(use-package flycheck-cask
  :after flycheck
  :commands (flycheck-cask-setup)
  :init
  (progn
    (add-hook 'flycheck-mode-hook #'flycheck-cask-setup)))

(use-package flycheck-package
  :after flycheck
  :config
  (progn
    (flycheck-package-setup)))

(use-package org
  :bind
  (("C-. o t"   . org-capture)
   ("C-. o a"   . org-agenda)
   ("C-. o l"   . org-store-link)
   ("C-. o w"   . my:org-move-to-refile-target)
   ("C-. o s"   . org-save-all-org-buffers))
  :init
  (progn
    (setq org-babel-load-languages '((shell . t) (emacs-lisp . t) (dot . t)))
    (setq org-catch-invisible-edits 'error)
    (setq org-clock-clocked-in-display nil)
    (setq org-completion-use-ido t)
    (setq org-directory "~/Documents/configuration/org")
    (setq org-default-notes-file (expand-file-name "refile.org" org-directory))
    (setq org-ellipsis "⤵")
    (setq org-export-allow-bind-keywords t)
    (setq org-export-creator-string "")
    (setq org-export-with-toc nil)
    (setq org-fontify-done-headline t)
    (setq org-hide-leading-stars t)
    (setq org-html-postamble nil)
    (setq org-imenu-depth 2)
    (setq org-log-done 'time)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-special-ctrl-a/e t)
    (setq org-startup-align-all-tables t)
    (setq org-table-use-standard-references nil)
    (setq org-time-stamp-rounding-minutes '(10 10))
    (setq org-use-speed-commands t)
    (setq org-email-link-description-format "%s"))
  :config
  (progn
    (setq org-modules '(org-protocol org-capture ox-beamer))

    (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

    (defun my:org-move-to-refile-target (&optional last)
      (interactive "p")
      (require 'org)
      (org-refile (if (= last 4) '(16) '(4))))

    ;; Custom agenda command definitions
    (setq org-agenda-custom-commands
          '((" " "Agenda"
             ((agenda "" nil)
              (tags "REFILE"
                    ((org-agenda-overriding-header "Tasks to Refile")
                     (org-tags-match-list-sublevels nil))))
             nil)))

    (setq org-default-calendar-file "~/Documents/configuration/org/schplaf.org")

    (add-to-list 'org-agenda-files "~/Documents/configuration/org/refile.org")
    (add-to-list 'org-agenda-files "~/Documents/configuration/org/tasks.org")
    (add-to-list 'org-agenda-files "~/Documents/configuration/org/repeating.org")
    (add-to-list 'org-agenda-files org-default-calendar-file)

    (setq org-refile-targets `((("~/Documents/configuration/org/repeating.org"
                                 "~/Documents/configuration/org/someday.org"
                                 "~/Documents/configuration/org/tasks.org")
                                :maxlevel . 2)))

    (setq org-todo-keywords
          '((sequence "TODO(t)" "WAIT(w)"   "|" "DONE(d)" "CANCELLED(c)")))

    (setq org-capture-templates
          '(("t" "Todo" entry
             (file org-default-notes-file)
             "* TODO %?%i")
            ("s" "Schedule" entry
             (file org-default-calendar-file)
             "* %?\n%^T")))

    ;; This is my `shell-switcher-switch-buffer':
    (unbind-key "C-'" org-mode-map)

    (with-eval-after-load 'org-agenda ;; Those are my `beginning-of-buffer' and `end-of-buffer':
      (unbind-key "<S-left>" org-agenda-mode-map)
      (unbind-key "<S-right>" org-agenda-mode-map))

    (unbind-key "<S-left>" org-mode-map)
    (unbind-key "<S-right>" org-mode-map)

    (add-to-list 'org-file-apps '("\\.png\\'" . default))))

(use-package org-notmuch
  :after org)

(use-package ox-twbs
  :after org)

(use-package org-caldav
  :bind (("C-. o S"   . org-caldav-sync))
  :config
  (progn
    (setq org-caldav-url "https://damien@petton.fr/nextcloud/remote.php/dav/calendars/damien"
          org-caldav-calendar-id "personal"
          org-caldav-inbox org-default-calendar-file
          org-caldav-files '()
          org-icalendar-timezone "Europe/Berlin"
          org-caldav-sync-changes-to-org 'all)))

(use-package calendar
  :init
  (progn
    (setq calendar-date-style 'european)
    (setq calendar-week-start-day 1)))

(use-package drag-stuff
  :demand t
  :diminish drag-stuff-mode
  :config
  (progn
    (drag-stuff-global-mode t)
    (drag-stuff-define-keys)
    (add-to-list 'drag-stuff-except-modes 'org-mode)
    (add-to-list 'drag-stuff-except-modes 'rebase-mode)
    (add-to-list 'drag-stuff-except-modes 'emacs-lisp-mode)))

(use-package expand-region
  :bind ("C-x =" . er/expand-region))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package shell-switcher
  :bind (("C-M-'"   . shell-switcher-new-shell)
         ("C-'"     . shell-switcher-switch-buffer)
         ("C-x 4 '" . shell-switcher-switch-buffer-other-window))
  :init
  (progn
    (setq shell-switcher-ask-before-creating-new t)
    (setq shell-switcher-new-shell-function 'shell-switcher-make-eshell))
  :config
  (progn
    (shell-switcher-mode)))

(use-package which-key
  :demand t
  :diminish which-key-mode
  :config
  (progn
    (which-key-mode)))

(use-package projectile
  :demand t
  :diminish projectile-mode
  :init
  (progn
    (setq projectile-completion-system 'helm)
    (setq projectile-keymap-prefix (kbd "C-. p"))
    (setq projectile-require-project-root nil))
  :config
  (progn
    (projectile-mode)

    (projectile-register-project-type
     'passwe
     '("package.json")
     :compile "make build"
     :test "make test"
     :test-suffix "-spec")

    (projectile-register-project-type
     'monitor
     '("gulpfile.js")
     :compile "cd monitor/Monitor.Web.Ui/Client && gulp lint:js"
     :test "cd monitor/Monitor.Web.Ui/Client && gulp karma"
     :test-suffix "-tests")))

(use-package helm-projectile
  :demand t
  :after projectile
  :bind (:map helm-projectile-projects-map
              ("M-s"     . my/helm-open-external-terminal))
  :config
  (progn
    (helm-projectile-on)
    (add-to-list 'helm-source-projectile-projects-actions
                 (cons "Open in external terminal `M-s'"
                       #'my/open-external-terminal)
                 t)))

(use-package unify-opening
  :demand t)

(eval-and-compile
  (setq-default notmuch-command (executable-find "notmuch")))

(use-package notmuch
  :if notmuch-command
  :bind (("C-. m" . notmuch)
         ("C-. M" . notmuch-mua-new-mail))
  :init
  (progn
    (setq notmuch-always-prompt-for-sender t)
    (setq notmuch-archive-tags '("-inbox" "-unread"))
    (setq notmuch-crypto-process-mime t)
    (setq notmuch-hello-sections '(notmuch-hello-insert-saved-searches))
    (setq notmuch-labeler-hide-known-labels t)
    (setq notmuch-search-oldest-first nil)
    (setq notmuch-show-imenu-indent t)
    (setq notmuch-draft-save-plaintext t))
  :config
  (progn
    (setq notmuch-archive-tags '("-inbox" "-unread"))

    (setq notmuch-message-headers '("To" "Cc" "Subject" "Date"))

    (defun my:mm-ics-to-org-part (handle &optional prompt)
      "Add message part HANDLE to org."
      (mm-with-unibyte-buffer
        (mm-insert-part handle)
        (mm-add-meta-html-tag handle)
        (require 'org-caldav)
        (org-caldav-import-ics-buffer-to-org)))

    (defun my:notmuch-show-ics-to-org-part ()
      "Save the .ics MIME part containing point to an org file."
      (interactive)
      (notmuch-show-apply-to-current-part-handle #'my:mm-ics-to-org-part))

    (with-eval-after-load "notmuch-show"
      (bind-key "d" #'my:notmuch-show-ics-to-org-part notmuch-show-part-map)
      ;; bind 'r' to reply-all, and 'R' to reply
      (bind-key "r" #'notmuch-search-reply-to-thread notmuch-search-mode-map)
      (bind-key "R" #'notmuch-search-reply-to-thread-sender notmuch-search-mode-map)
      (bind-key "r" #'notmuch-show-reply notmuch-show-mode-map)
      (bind-key "R" #'notmuch-show-reply-sender notmuch-show-mode-map)
      (bind-key "a" #'nico-notmuch-git-am-patch notmuch-show-part-map))

    (defun nico-notmuch-git-am-patch ()
      "Apply the MIME part at point as a git patch using `git am'."
      (interactive)
      (notmuch-show-apply-to-current-part-handle #'nico-notmuch-git-am-part))

    (defun nico-notmuch-git-am-part (handle)
      (let ((dir (read-directory-name "Git directory: ")))
        (mm-pipe-part handle (format "cd %s; git am" (expand-file-name dir)))))))

;; (defun my/notmuch-wash-merge-lines-in-format=flowed (msg depth)
;;   "Must be executed before `notmuch-wash-wrap-long-lines'."
;;   (save-excursion
;;     (goto-char (point-min))))

(use-package profile
  :after (notmuch)
  :init
  (progn
    (with-eval-after-load "message"
      (bind-key "C-c F" #'profile-force-profile-in-compose message-mode-map)))
  :config
  (progn
    (setq profile-binding-alist
          '(("Perso"
             (profile-maildir . "/Perso")
             (notmuch-fcc-dirs . "Perso/Sent")
             (user-mail-address . "damien@cassou.me")
             (message-signature . t)
             (smtpmail-queue-dir . "~/Mail/Perso/queued-mail/")
             (smtpmail-local-domain . nil)
             (smtpmail-smtp-user . "damien@cassou.me")
             (smtpmail-smtp-server . "choca.pics")
             (smtpmail-stream-type . ssl)
             (smtpmail-smtp-service . 465))
            ("Ftgp"
             (profile-maildir . "/Ftgp")
             (notmuch-fcc-dirs . "Ftgp/Sent")
             (user-mail-address . "damien.cassou@foretagsplatsen.se")
             (message-signature . "Damien Cassou\nFöretagsplatsen AB\nPhone/Fax: +46 (0)8 774 63 00\nMobile: +33 (0)6 80 50 18 91\nAddress: Skeppsbron 26, 4tr, SE-111 30 Stockholm\nWeb: www.foretagsplatsen.se\n")
             (smtpmail-queue-dir . "~/Mail/Ftgp/queued-mail/")
             (smtpmail-local-domain . nil)
             (smtpmail-smtp-user . "damien.cassou@foretagsplatsen.se")
             (smtpmail-smtp-server . "smtp.office365.com")
             (smtpmail-stream-type . starttls)
             (smtpmail-smtp-service . 587))))
    (profile-set-profile-from-name "Perso")
    (setq profile-extra-email-addresses
          '("damien.cassou@lifl.fr" "cassou@inria.fr"
            "damien.cassou@laposte.net" "damien@foretagsplatsen.se"))
    (setq profile-noisy-query
          "to:\"notmuch@notmuchmail.org\" OR to:\"offlineimap-project@lists.alioth.debian.org\" OR to:\"emacs-devel\" OR to:\"dev-addons@mozilla.org\" OR to:\"gnupg-users@gnupg.org\"")

    (defun my:notmuch-build-identity (&optional email)
      "Return a string of the form \"name <EMAIL>\"."
      (let ((email (or email user-mail-address)))
        (format "%s <%s>" (notmuch-user-name) email)))

    (setq notmuch-identities
          (mapcar #'my:notmuch-build-identity
                  (profile-email-addresses)))

    (defun my:notmuch-prompt-for-sender ()
      "Prompt for a sender using `profile-binding-alist'."
      (profile-set-profile)
      (my:notmuch-build-identity))

    (advice-add #'notmuch-mua-prompt-for-sender
                :override
                #'my:notmuch-prompt-for-sender)

    ;; https://notmuchmail.org/pipermail/notmuch/2017/025320.html
    (defun my:notmuch-mua-new-reply (arguments)
      "Always set PROMPT-FOR-SENDER to t when using `notmuch-mua-new-reply'."
      (list (cl-first arguments) t (cl-third arguments)))

    (advice-add #'notmuch-mua-new-reply :filter-args #'my:notmuch-mua-new-reply)

    (setq notmuch-saved-searches
          `((:name "inbox" :query ,(format "(folder:\"Perso/INBOX\") AND (NOT (%s) OR recip:damien*)"
                                           profile-noisy-query) :key "i")
            (:name "noisy" :query ,(profile-noisy-unarchived-list-query) :key "n")
            (:name "ftgp" :query "folder:\"Ftgp/INBOX\" AND tag:inbox" :key "f")
            (:name "sent" :query ,(profile-sent-query) :key "s")))

    (add-to-list 'notmuch-hello-sections
                 #'profile-queue-insert-section
                 t)))

(use-package mml
  :config
  (progn
    ;; http://mbork.pl/2015-11-28_Fixing_mml-attach-file_using_advice
    (defun my:mml-attach-file--go-to-eob (orig-fun &rest args)
      "Go to the end of buffer before attaching files."
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-max))
          (apply orig-fun args))))

    (advice-add 'mml-attach-file :around #'my:mml-attach-file--go-to-eob)))

(use-package message
  :init
  (progn
    (setq message-default-charset 'utf-8)
    (setq message-log-max t)
    (setq message-send-mail-function 'message-smtpmail-send-it)
    (setq message-signature t)
    (setq message-signature-file "~/.signature"))
  :config
  (progn
    ;;; The following make sure to use the right profile when sending
    ;;; the message (i.e., when pressing C-c C-c in message-mode).
    ;;; It's better to set the profile just before sending to be sure
    ;;; to use the profile related to the From: message field).
    (defun my:message-send-and-exit (&optional arg)
      "Set profile according to From field.
Designed to be called before `message-send-and-exit'."
      (require 'profile)
      (profile-set-profile-from-message-from-field))
    (advice-add #'message-send-and-exit
                :before
                #'my:message-send-and-exit)))

(use-package image
  :config
  (imagemagick-register-types))

(use-package paredit
  :diminish paredit-mode
  :commands (enable-paredit-mode)
  :bind (:map paredit-mode-map
              ("M-s" . nil))
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode))
  :config
  (progn
    (with-eval-after-load "eldoc"
      (eldoc-add-command #'paredit-backward-delete #'paredit-close-round))))

(use-package autorevert
  :diminish auto-revert-mode)

(use-package smartscan
  :commands (smartscan-mode)
  :init
  (progn
    (add-hook 'prog-mode-hook #'smartscan-mode)
    (add-hook 'pillar-mode-hook #'smartscan-mode)))

(use-package zoom-frm
  :bind (("C-x C-+" . zoom-in/out)
         ("C-x C--" . zoom-in/out)
         ("C-x C-=" . zoom-in/out)
         ("C-x C-0" . zoom-in/out)
         ("<C-mouse-4>" . zoom-in)
         ("<C-mouse-5>" . zoom-out))
  :init
  (progn
    (setq zoom-frame/buffer 'frame)))

(use-package visible-mark
  :demand t
  :init
  (progn
    (setq visible-mark-faces '(visible-mark-face1 visible-mark-face2))
    (setq visible-mark-max 2))
  :config
  (progn
    (global-visible-mark-mode)))

(use-package paren
  :demand t
  :config
  (progn
    (show-paren-mode)))

(use-package paren-face
  :demand t
  :config
  (progn
    (global-paren-face-mode)))

(use-package help
  :bind (:map help-map
              ("C-v" . find-variable)
              ("C-k" . find-function-on-key)
              ("C-f" . find-function)
              ("C-l" . find-library)
              :map help-mode-map
              ("g" . my:revert-buffer-no-confirm))
  :config
  (progn
    (defun my:revert-buffer-no-confirm (&optional ignore-auto)
      "Revert current buffer without asking."
      (interactive (list (not current-prefix-arg)))
      (revert-buffer ignore-auto t nil))))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h k" . helpful-key)))

(use-package anzu
  :demand t
  :diminish anzu-mode
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (progn
    (global-anzu-mode +1)
    (define-key isearch-mode-map [remap isearch-query-replace] #'anzu-isearch-query-replace)
    (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)))

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :commands aggressive-indent-mode
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
    (add-hook 'lisp-mode-hook #'aggressive-indent-mode)))

(use-package helm
  :demand t
  :diminish helm-mode
  :bind (("M-x"     . helm-M-x)
         ("M-y"     . helm-show-kill-ring)
         ("C-x b"   . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x d"   . helm-find-files)
         ("C-h SPC" . helm-all-mark-rings)
         ("C-:"     . helm-eval-expression-with-eldoc)
         :map helm-map
         ("<tab>"   . helm-execute-persistent-action)
         ("C-i"     . helm-execute-persistent-action)
         ("C-z"     . helm-select-action)
         ("<S-left>"  . helm-beginning-of-buffer)
         ("<S-right>" . helm-end-of-buffer)
         :map helm-find-files-map
         ("M-s"     . my/helm-open-external-terminal))
  :init
  (progn
    (require 'helm-config))
  :config
  (progn
    (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
          helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
          helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
          helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
          helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
          helm-ff-file-name-history-use-recentf t)

    (with-eval-after-load "eshell"
      (require 'helm-eshell))

    (with-eval-after-load "projectile"
      (require 'helm-projectile))

    (helm-mode 1)

    (defun my/helm-open-external-terminal ()
      (interactive)
      (with-helm-alive-p
        (helm-exit-and-execute-action
         #'my/open-external-terminal)))

    (defun my/open-external-terminal (file)
      (interactive)
      (async-shell-command
       (concat "cd "
               (file-name-directory file)
               " && gnome-terminal")))

    (add-to-list 'helm-find-files-actions
                 (cons "Open in external terminal `M-s'"
                       #'my/open-external-terminal)
                 t)))

(use-package helm-descbinds
  :after helm
  :config
  (progn
    (helm-descbinds-mode)))

(use-package bookmark
  :init
  (progn
    (customize-save-variable 'bookmark-save-flag 1)))

(use-package helm-bookmark
  :bind (("C-x r b" . helm-filtered-bookmarks)
         :map helm-bookmark-map
         ("M-s" . my/helm-open-external-terminal))
  :config
  (progn
    (add-to-list 'helm-type-bookmark-actions
                 (cons "Open in external terminal `M-s'"
                       #'my/open-external-terminal)
                 t)))

(use-package counsel
  :bind (("M-i" . counsel-imenu)))

(use-package password-store
  :config
  (progn
    (setq password-store-password-length 30)))

(use-package pass
  :commands pass
  :init
  (progn
    (defun my/pass-insert-generated (entry)
      "Same as pass-insert-generated but with my own template."
      (interactive (list (read-string "Password entry: ")))
      (when (or (not (seq-contains (password-store-list) entry))
                (yes-or-no-p "Erase existing entry with same name? "))
        (let ((password (shell-command-to-string
                         (format "pwgen --secure --symbols %s"
                                 password-store-password-length))))
          (password-store-insert
           entry
           (format "%s--\nusername: %s\nurl: https://%s\n"
                   password
                   user-mail-address
                   entry))
          (password-store-edit entry)
          (pass-update-buffer)))))
  :config
  (progn
    (advice-add #'pass-insert-generated :override #'my/pass-insert-generated)))

(use-package auth-source
  :init
  (progn
    (setq auth-source-debug t)
    (setq auth-source-do-cache nil)))

(use-package auth-password-store
  :after auth-source
  :init
  (progn
    (setq auth-sources '(password-store))))

(use-package ace-window
  :bind* (("M-o" . ace-window))
  :init
  (progn
    ;; keys under my fingers (blue keys on my Kinesis Advantage)
    (setq aw-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o 59 32))
    (setq aw-scope 'frame)))

(use-package avy
  :bind* (("C-," . avy-goto-char-2))
  ;; dont :bind* this as this would override `M-g` in Projectile
  ;; (helm-projectile-vc):
  :bind (("M-g g" . avy-goto-line))
  :init
  (progn
    ;; keys under my fingers (blue keys on my Kinesis Advantage)
    (setq avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o 59 32))
    (setq avy-style 'at-full)))

(use-package outline
  :diminish outline-minor-mode)

(use-package beginend
  :demand t
  :diminish beginend-global-mode
  :config
  (progn
    (beginend-global-mode)))

(use-package vdirel
  :bind (("C-. c" . vdirel-helm-select-email))
  :init
  (progn
    (setq vdirel-repository
          "~/Documents/configuration/contacts/contacts")))

(use-package nameless
  :diminish nameless-mode
  :commands (nameless-mode)
  :init
  (progn
    (setq nameless-affect-indentation-and-filling nil)
    (setq nameless-prefix "…")
    (add-hook 'emacs-lisp-mode-hook #'nameless-mode)))

(use-package beacon
  :demand t
  :diminish beacon-mode
  :init
  (progn
    (setq beacon-blink-when-focused t))
  :config
  (progn
    ;; don't blink in notmuch-search, it's both slow and ugly
    (add-to-list 'beacon-dont-blink-major-modes #'notmuch-search-mode)
    (beacon-mode)))

(use-package subword
  :diminish subword-mode
  :init
  (progn
    (global-subword-mode)))

(use-package company
  :diminish company-mode
  :commands (company-mode)
  :bind ("C-. /" . company-complete)
  :init
  (progn
    (setq company-dabbrev-downcase nil)
    (setq company-dabbrev-ignore-case nil)
    (add-hook 'emacs-lisp-mode-hook #'company-mode)
    (add-hook 'css-mode-hook #'company-mode)))

(use-package emacs-js
  :diminish (js2-refactor-mode js2-minor-mode js-lint-mode tern-mode)
  :load-path "packages/emacs-js"
  :commands (setup-js-buffer)
  :init
  (progn
    (add-hook 'js-mode-hook #'setup-js-buffer))
  :config
  (progn
    (setenv "PATH" (concat (getenv "PATH") ":/home/cassou/node_modules/.bin"))
    (add-to-list 'exec-path "/home/cassou/node_modules/.bin")))

(use-package widgetjs
  :load-path "packages/emacs-js/widgetjs"
  :diminish widgetjs-mode)

(use-package amd-mode
  :diminish amd-mode)

(use-package indium
  :diminish indium-interaction-mode
  :init
  (progn
    (setq indium-update-script-on-save nil)
    (setq indium-workspace-file (no-littering-expand-var-file-name "indium-workspaces.el"))))

(use-package gulp-task-runner
  :commands (gulp))

(use-package ftgp
  :demand t
  :load-path "packages/ftgp"
  :config
  (progn
    (require 'bookmark)
    (setq ftgp-atlassian-pass-entry "ftgp/id.atlassian.com_(API)")
    (setq ftgp-monitor-root-location
          (expand-file-name (bookmark-location "ftgp-monitor-root")))))

(use-package jabber
  :disabled t
  :bind
  (("C-. j c" . jabber-connect-all)
   ("C-. j d" . jabber-disconnect)
   ("C-. j r" . jabber-switch-to-roster-buffer)
   ("C-. j j" . jabber-chat-with)
   ("C-. j l" . jabber-activity-switch-to)
   ("C-. j a" . jabber-send-away-presence)
   ("C-. j o" . jabber-send-default-presence)
   ("C-. j x" . jabber-send-xa-presence)
   ("C-. j p" . jabber-send-presence))
  :init
  (progn
    (setq jabber-auto-reconnect t)
    (setq jabber-backlog-days 30)
    (setq jabber-backlog-number 100)
    (setq jabber-history-enabled t))
  :config
  (progn
    (setq jabber-account-list
          `(("damien@cassou.me"
             (:password . ,(password-store-get "ldn-fai.net")))))
    (add-hook 'jabber-post-connect-hooks #'jabber-autoaway-start)
    (add-hook 'jabber-chat-mode-hook #'flyspell-mode)
    (with-eval-after-load "guess-language"
      (add-hook 'jabber-chat-mode-hook #'guess-language-mode))

    ;; Override jabber.el global key
    (bind-key "C-x C-j" #'dired-jump)))

(use-package lui
  :config
  (progn
    (setq lui-flyspell-p t)
    (setq lui-flyspell-alist '((".*" "american")))

    (setq lui-time-stamp-position 'right-margin)
    (setq lui-fill-type nil)
    (setq lui-time-stamp-format "%H:%M")

    (defun my/lui-setup ()
      (setq right-margin-width 5)
      (setq fringes-outside-margins t)
      (setq word-wrap t)
      (setq wrap-prefix "    "))
    (add-hook 'lui-mode-hook 'my/lui-setup)))

(use-package lui-logging
  :after lui
  :config
  (progn
    (enable-lui-logging-globally)))

(use-package tracking
  :bind (("C-. ." . tracking-next-buffer))
  :config
  (progn
    (setq tracking-mode-map (make-sparse-keymap))))

(use-package circe
  ;; Install gnutls-utils if circe is stuck "Connecting..."
  ;; https://github.com/jorgenschaefer/circe/issues/287
  :init
  (progn
    (defun my/get-password (host &optional user)
      "Return password for HOST and USER."
      (when-let ((entry (auth-pass--find-match host user)))
        (auth-pass-get 'secret entry))))
  :config
  (progn
    (setq circe-default-nick "DamienCassou")
    (setq circe-reduce-lurker-spam t)
    (setq-default circe-sasl-username "DamienCassou")
    (setq-default circe-nickserv-nick "DamienCassou")
    (setq-default circe-sasl-password #'my/get-password)
    (setq-default circe-nickserv-password #'my/get-password)
    (setq circe-format-say "{nick:-16s} {body}")

    (add-to-list 'circe-network-defaults
                 '("Mozilla"
                   :host "irc.mozilla.org" :port (6667 . 6697)
                   :tls t
                   :nickserv-mask "^NickServ!NickServ@services\\.$"
                   :nickserv-identify-challenge "This nickname is registered and protected."
                   :nickserv-identify-command "PRIVMSG NickServ IDENTIFY {password}"
                   :nickserv-identify-confirmation "^You are now identified for .*\\.$"
                   :nickserv-ghost-command "PRIVMSG NickServ :GHOST {nick} {password}"
                   :nickserv-ghost-confirmation "has been ghosted\\.$\\|is not online\\.$"))
    (add-to-list 'circe-network-defaults
                 '("Gnome"
                   :host "irc.gnome.org" :port (6667 . 6697)
                   :tls t
                   :nickserv-mask "^NickServ!NickServ@services\\.$"
                   :nickserv-identify-challenge "This nickname is registered and protected."
                   :nickserv-identify-command "MSG NickServ IDENTIFY {password}"
                   :nickserv-identify-confirmation "^You are now identified for .*\\.$"
                   :nickserv-ghost-command "PRIVMSG NickServ :GHOST {nick} {password}"
                   :nickserv-ghost-confirmation "has been ghosted\\.$\\|is not online\\.$"))))

(use-package circe-notifications
  :disabled t
  :after circe
  :config
  (progn
    (add-hook 'circe-server-connected-hook 'enable-circe-notifications)))

(use-package alert
  :config
  (progn
    (setq alert-user-configuration '((nil notifications nil)))))

(use-package erc-track
  :init
  (progn
    (setq erc-track-enable-keybindings nil)))

(use-package diff-hl
  :commands (diff-hl-mode diff-hl-magit-post-refresh)
  :init
  (progn
    (add-hook 'prog-mode-hook #'diff-hl-mode)
    (with-eval-after-load 'magit
      (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh t))))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-minor-mode)
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'yas-minor-mode))
  :config
  (progn
    (delq 'yas-installed-snippets-dir yas-snippet-dirs)
    (add-to-list 'yas-snippet-dirs "~/.emacs.d/packages/yasnippet-snippets")
    (yas-reload-all)))

(use-package ws-butler
  :diminish ws-butler-mode
  :commands (ws-butler-mode)
  :init
  (progn
    (add-hook 'prog-mode-hook #'ws-butler-mode)))

(use-package editorconfig
  :diminish editorconfig-mode
  :init
  (progn
    (add-hook 'prog-mode-hook #'editorconfig-mode)
    (add-hook 'text-mode-hook #'editorconfig-mode)))

(use-package compile
  :config
  (progn
    ;; http://stackoverflow.com/questions/13397737
    (defun my/colorize-compilation-buffer ()
      (require 'ansi-color)
      (toggle-read-only)
      (ansi-color-apply-on-region compilation-filter-start (point))
      (toggle-read-only))
    (add-hook 'compilation-filter-hook #'my/colorize-compilation-buffer)))

(use-package duplicate-thing
  :bind (("M-D" . duplicate-thing)))

(use-package embrace
  :bind (("C-. ," . embrace-commander))
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'embrace-emacs-lisp-mode-hook)))

(use-package offlineimap
  :commands (offlineimap)
  :init
  (progn
    (setq offlineimap-command "offlineimap -u machineui"))
  :config
  (progn
    (defun my/offlineimap-message-when-done (message-type &optional action)
      (if (string-match "^finished" message-type)
          (message "Offlineimap finished")
        (message "Offlineimap in progress")))
    (add-hook 'offlineimap-event-hooks #'my/offlineimap-message-when-done)))

(use-package smtpmail
  :init
  (progn
    (setq smtpmail-debug-info t)
    (setq smtpmail-debug-verb t)
    (setq smtpmail-queue-mail nil)
    (setq smtpmail-stream-type 'starttls)))

(use-package sendmail
  :init
  (progn
    (setq send-mail-function 'smtpmail-send-it)))

(use-package google-translate
  :init
  (progn
    (setq google-translate-default-source-language "sv")
    (setq google-translate-default-target-language "en")))

(use-package helm-flyspell
  :bind (("M-$" . helm-flyspell-correct)))

(use-package my-misc
  :demand t
  :load-path "lisp")

(use-package messages-are-flowing
  :disabled t
  :demand t
  :config
  (progn
    (add-hook 'message-mode-hook
              #'messages-are-flowing-use-and-mark-hard-newlines)))

(use-package make-it-so)

(use-package snapshot-timemachine-rsnapshot
  :after snapshot-timemachine
  :config
  (progn
    (setq snapshot-timemachine-rsnapshot-backup-dir
          (expand-file-name
           "rsnapshot"
           (bookmark-get-filename "Lacie")))))

(use-package skeletor
  :commands (skeletor-create-project)
  :init
  (progn
    (setq skeletor-project-directory "/home/cassou/.emacs.d/packages/")
    (setq skeletor-show-project-command 'magit-status)))

(use-package buttercup)

(use-package hierarchy)

(use-package klassified
  :load-path "~/.emacs.d/packages/klassified"
  :commands (klassified-interaction-js-mode)
  :init
  (progn
    (add-hook 'js-mode-hook #'klassified-interaction-js-mode)))

(use-package json-navigator
  :commands (json-navigator-navigate-region json-navigator-navigate-after-point))

(use-package markdown-mode
  :init
  (progn
    (setq markdown-command "kramdown")))

(use-package hydra
  :init
  (progn
    (defhydra hydra-window (global-map "C-. w")
      "zoom"
      ("+" zoom-frm-in "in")
      ("-" zoom-frm-out "out")
      ("=" zoom-frm-unzoom "reset"))))

(use-package devdocs
  :commands (devdocs-search))

(use-package websocket
  :config
  (progn
    (setq websocket-callback-debug-on-error t)
    (setq websocket-debug t)))

(use-package firestarter
  :demand t
  :config
  (progn
    (add-hook 'prog-mode-hook #'firestarter-mode)
    (setq firestarter-default-type 'finished)))

(use-package adoc-mode
  :mode "\\.adoc\\'")

(use-package bash-completion
  :init
  (progn
    (add-hook 'shell-dynamic-complete-functions
              #'bash-completion-dynamic-complete)))

(use-package eshell
  :commands (eshell eshell-command)
  :preface
  (progn
    (defun my/eshell-prompt ()
      (let ((path (abbreviate-file-name (eshell/pwd))))
        (let* ((background "#3d4a41") (foreground "#abb2bf"))
          (concat
           (format
            (propertize "%s\n>"
                        'face `(:foreground "#98be65" :background ,background :weight bold))
            (propertize path
                        'face `(:foreground ,foreground :background ,background)))
           " "))))

    (defun eshell-bash-completion ()
      (require 'bash-completion)
      (while (pcomplete-here
              (nth 2 (bash-completion-dynamic-complete-nocomint
                      (save-excursion (eshell-bol) (point))
                      (point))))))

    (defun my/eshell-mode-configure ()
      (eshell-cmpl-initialize)
      (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
      (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))
  :config
  (progn
    ;; Inspired from
    ;; https://github.com/Ambrevar/dotfiles/blob/master/.emacs.d/lisp/init-eshell.el
    (setq eshell-history-size 1024)
    (setq eshell-hist-ignoredups t)
    (setq eshell-prompt-function #'my/eshell-prompt)

    ;; If the prompt spans over multiple lines, the regexp should match
    ;; last line only.
    (setq-default eshell-prompt-regexp "^> ")

    (use-package em-term
      :config
      (progn
        (nconc eshell-visual-commands
               '("htop" "pinentry-curses" "watch"))
        (nconc eshell-visual-subcommands
               '(("git" "log" "diff" "show")
                 ("npm" "install")
                 ("docker" "build")))))

    (add-hook 'eshell-mode-hook #'my/eshell-mode-configure)

    (use-package bash-completion
      :init
      (progn
        (setq eshell-default-completion-function 'eshell-bash-completion)))))

(use-package graphviz-dot-mode)

;; Local Variables:
;; eval: (outline-minor-mode)
;; eval: (flycheck-mode -1)
;; End:
