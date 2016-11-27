;;; -*- Mode: Emacs-Lisp -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-debug t)
 '(auth-source-do-cache nil nil nil "Don't save as I'm doing experiments with gnupg")
 '(avy-style (quote at-full))
 '(aw-scope (quote frame))
 '(beacon-blink-when-focused t)
 '(bookmark-save-flag 1)
 '(calendar-date-style (quote european))
 '(calendar-week-start-day 1)
 '(checkdoc-spellcheck-documentation-flag t)
 '(column-number-mode t)
 '(confirm-kill-emacs (quote y-or-n-p))
 '(current-language-environment "UTF-8")
 '(custom-safe-themes t)
 '(delete-active-region nil)
 '(delete-by-moving-to-trash t)
 '(dired-dwim-target t)
 '(dired-listing-switches "-alh")
 '(dired-recursive-deletes (quote always))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(electric-pair-mode t)
 '(enable-local-variables :all)
 '(enable-recursive-minibuffers t)
 '(erc-autojoin-channels-alist (quote (("freenode.net" "#fedora" "#emacs"))))
 '(erc-nick "DamienCassou")
 '(eval-expression-print-length 20)
 '(eval-expression-print-level nil)
 '(flyspell-use-meta-tab nil)
 '(frame-title-format "Emacs: %b" t)
 '(gc-cons-threshold 20000000)
 '(global-font-lock-mode t)
 '(global-pair-mode t)
 '(global-prettify-symbols-mode t)
 '(global-subword-mode t)
 '(global-undo-tree-mode t)
 '(guide-key/guide-key-sequence t)
 '(imenu-auto-rescan t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(initial-major-mode (quote text-mode))
 '(initial-scratch-message nil)
 '(jabber-auto-reconnect t)
 '(jabber-avatar-cache-directory "~/.emacs.d/cache/jabber-avatar-cache")
 '(jabber-backlog-days 30)
 '(jabber-backlog-number 100)
 '(jabber-history-enabled t)
 '(load-prefer-newer t)
 '(magit-diff-refine-hunk t)
 '(magit-process-find-password-functions (quote (magit-process-password-auth-source)))
 '(magit-wip-after-apply-mode nil)
 '(magit-wip-after-save-mode nil)
 '(magit-wip-before-change-mode nil)
 '(make-backup-files nil)
 '(markdown-command "Markdown.pl")
 '(menu-bar-mode nil)
 '(message-default-charset (quote utf-8))
 '(message-log-max t)
 '(message-send-mail-function (quote message-smtpmail-send-it))
 '(message-signature t)
 '(message-signature-file "~/.signature")
 '(next-screen-context-lines 5)
 '(notmuch-always-prompt-for-sender t)
 '(notmuch-archive-tags (quote ("-inbox" "-unread")))
 '(notmuch-crypto-process-mime t)
 '(notmuch-hello-sections (quote (notmuch-hello-insert-saved-searches)))
 '(notmuch-labeler-hide-known-labels t)
 '(notmuch-search-oldest-first nil)
 '(org-babel-load-languages (quote ((sh . t) (emacs-lisp . t) (java . t) (python . t))))
 '(org-catch-invisible-edits (quote error))
 '(org-clock-clocked-in-display nil)
 '(org-completion-use-ido t)
 '(org-default-notes-file "refile.org")
 '(org-directory "~/Documents/configuration/org")
 '(org-ellipsis "⤵")
 '(org-export-allow-bind-keywords t)
 '(org-export-creator-string "")
 '(org-export-with-toc nil)
 '(org-fontify-done-headline t)
 '(org-hide-leading-stars t)
 '(org-html-postamble nil)
 '(org-imenu-depth 2)
 '(org-log-done (quote time))
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-use-outline-path (quote full-file-path))
 '(org-special-ctrl-a/e t)
 '(org-startup-align-all-tables t)
 '(org-table-use-standard-references nil)
 '(org-time-stamp-rounding-minutes (quote (10 10)))
 '(org-use-speed-commands t)
 '(package-archive-priorities (quote (("melpa-stable" . 10))))
 '(powerline-display-buffer-size nil)
 '(proced-filter (quote all))
 '(projectile-cache-file "/home/cassou/.emacs.d/cache/projectile.cache")
 '(projectile-completion-system (quote helm))
 '(projectile-keymap-prefix (kbd "C-. p"))
 '(projectile-known-projects-file "/home/cassou/.emacs.d/cache/projectile-bookmarks.eld")
 '(projectile-require-project-root nil)
 '(read-file-name-completion-ignore-case t)
 '(recentf-auto-cleanup 300)
 '(recentf-exclude (quote ("~$" "\\.log$")))
 '(recentf-max-saved-items 4000)
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/recentf")
 '(report-emacs-bug-no-explanations t)
 '(runner-run-in-background t)
 '(save-interprogram-paste-before-kill t)
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/places")
 '(scheme-program-name "petite")
 '(scroll-bar-mode nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(sh-indent-comment t)
 '(shell-switcher-ask-before-creating-new t)
 '(shell-switcher-mode t)
 '(shell-switcher-new-shell-function (quote shell-switcher-make-eshell))
 '(show-paren-mode t)
 '(skeletor-project-directory "/home/cassou/.emacs.d/packages/")
 '(skeletor-show-project-command (quote magit-status))
 '(smart-tab-completion-functions-alist nil)
 '(smart-tab-using-hippie-expand t)
 '(smtpmail-default-smtp-server "smtp.gmail.com")
 '(smtpmail-queue-mail nil)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587)
 '(smtpmail-smtp-user "damien.cassou@gmail.com")
 '(smtpmail-stream-type (quote starttls))
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(truncate-partial-width-windows nil)
 '(undo-limit 5000000)
 '(undo-outer-limit 200000000)
 '(undo-strong-limit 10000000)
 '(undo-tree-auto-save-history t)
 '(undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/.undo-tree/"))))
 '(undo-tree-mode-lighter "")
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(use-package-always-defer t)
 '(use-package-always-pin "melpa-stable")
 '(use-package-enable-imenu-support t)
 '(use-package-minimum-reported-time 0)
 '(use-package-verbose t)
 '(user-full-name "Damien Cassou")
 '(user-mail-address "damien.cassou@gmail.com")
 '(vc-follow-symlinks nil)
 '(vdirel-repository "~/Documents/configuration/contacts")
 '(version-control t)
 '(visible-bell nil)
 '(visible-mark-faces (quote (visible-mark-face1 visible-mark-face2)))
 '(visible-mark-max 2)
 '(winner-mode t nil (winner) "Use C-c <left|right> to go back to previous windows configuration")
 '(zoom-frame/buffer (quote frame)))

(setq package-selected-packages
      '(
        ace-link ; type o in help-mode to go to a link
        ace-window ; manage windows with ace-like behavior
        ag ; search using the 'ag' command (better grep)
        aggressive-indent ; indent code automatically while typing
        all-the-icons ; library with many icons (zerodark dependency)
        anzu ; more interactive query-replace
        assess ; library to facilitate test writing
        avy ; move fast in buffer with <C-,>
        bind-key ; to simplify definition of shortcuts
        camcorder ; record emacs sessions M-x camcorder-record
        company-tern ; tern backend for company mode
        csharp-mode ; C# major mode
        dash ; list library
        diff-hl ; shows git status in buffer's fringe
        diminish ; Shorter mode names in the modeline
        dired-imenu ; integrates imenu in dired
        dired-toggle-sudo ; <C-x s> to toggle sudo state of buffer
        discover ; popus for some features
        drag-stuff ; use <M-arrow> to move things around
        editorconfig ; handle .editorconfig files automatically
        expand-region ; <C-x => repeadly to mark regions
        f ; file manipulation library
        feature-mode ; major mode for editing feature files
        flycheck ; flycheck to check files on the fly
        flycheck-cask ; use Cask when present for dependencies
        flycheck-package ; checks elisp package metadata
        git-timemachine ; history of a file with M-x git-timemachine
        grunt ; glue for grunt files (Javascript)
        guide-key ; help you learn emacs
        helm ; selection/completion interface for everything
        helm-ag ; use ag from helm
        helm-descbinds ; list available bindings through helm
        helm-projectile ; integrate projectile and helm <C-. p h>
        jabber ; instant messaging
        js2-mode ; Improved Javascript editing mode
        js2-refactor ; A Javascript refactoring library
        json-mode ; Major mode to edit JSON files
        less-css-mode ; Major mode to edit .less files
        magit ; Integrate git <C-x g>
        markdown-mode ; Major mode for markdown format
        multiple-cursors ; Control multiple cursors with <C-S-c C-S-c>
        notmuch ; email client
        org-vcard ; used by vdirel
        orgtbl-show-header ; show header of column in minibuffer
        ox-twbs ; use twitter bootstrap to export org files to HTML
        paredit ; edit lisp AST instead of characters
        paren-face ; hide parenthesis in elisp code
        pass ; Nicolas' major mode for password-store
        pos-tip ; make tool-tips appear nicely
        prodigy ; manage external services from within Emacs
        projectile ; add notion of projects
        refine ; edit list interactively
        runner ; Associate external applications to file extensions
        s ; string library
        skeletor ; facilitates the creation of new project
        ;; slack  ; slack.com client for Emacs
        smartscan ; <M-n> <M-p> to move between same symbol in buffer
        tern ; Javascript code analyzer
        undo-tree ; <C-x u> to show the undo tree
        visible-mark ; show the current mark
        ws-butler ; trim whitespace only in edited lines
        xref-js2 ; Jump to references/definitions using ag &
                                        ; js2-mode's AST (JavaScript)
        yaml-mode ; to edit *.yml files (including .travis.yml)
        yasnippet ; expand snippets of text
        org-caldav ; sync between org and caldav
        password-store ; get passwords from the 'pass' command
        use-package ; to structure my init.el file
        zoom-frm ; change font size for all buffers <C-x C-+>
        beacon ; highlight my cursor when scrolling
        debbugs ; SOAP library to access debbugs servers
        nameless ; hide current package name everywhere in elisp code
        ))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Do that when you want to install packages of
;; `package-selected-packages':
;;
;; (package-install-selected-packages)

(require 'diminish)
(require 'use-package)

(use-package undo-tree
  :demand t
  :config
  (progn
    (global-undo-tree-mode)
    (define-key undo-tree-map (kbd "C-x r") nil)))

(use-package dired
  :bind (("C-x C-j" . dired-jump))
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
  :after dired)

(use-package dired-x
  :after dired)

(use-package dired-imenu
  :after dired)

(use-package recentf
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
    (add-hook 'dired-mode-hook 'recentf-track-dired-buffers t)))

(use-package em-term
  :config
  (progn
    (add-to-list 'eshell-visual-commands "htop")
    (add-to-list 'eshell-visual-commands "journalctl")
    (add-to-list 'eshell-visual-commands "karma")
    (add-to-list 'eshell-visual-commands "bower")))

(use-package magit
  :diminish (magit-auto-revert-mode magit-wip-after-save-mode magit-wip-after-apply-mode magit-wip-affter-change)
  :bind (("C-x g" . magit-status) ("C-x G" . magit-dispatch-popup))
  :config
  (progn
    ;; Enable magit-clean
    (put 'magit-clean 'disabled nil)))

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
  :diminish checkdoc-minor-mode)

(use-package face-remap
  :diminish text-scale-mode)

(use-package flycheck
  :diminish flycheck-mode
  :commands (flycheck-mode)
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'flycheck-mode)))

(use-package flycheck-cask
  :after flycheck
  :commands (flycheck-cask-setup)
  :init
  (progn
    (add-hook 'flycheck-mode-hook #'flycheck-cask-setup)))

(use-package org
  :bind
  (("C-. o t"   . org-capture)
   ("C-. o a"   . org-agenda)
   ("C-. o l"   . org-store-link)
   ("C-. o w"   . my:org-move-to-refile-target)
   ("C-. o s"   . org-save-all-org-buffers))
  :config
  (progn
    (setq org-modules '(org-protocol org-capture ox-beamer))

    (defun my:org-move-to-refile-target (&optional last)
      (interactive "p")
      (require 'org)
      (org-refile (if (= last 4) '(16) '(4))))

    (bind-key "<S-left>" #'beginning-of-buffer org-mode-map)
    (bind-key "<S-right>" #'end-of-buffer org-mode-map)

    ;; Custom agenda command definitions
    (setq org-agenda-custom-commands
          (quote (("l" "Logbook" nico/org-agenda-log ""
                   ((org-agenda-overriding-header "Logbook")))
                  (" " "Agenda"
                   ((agenda "" nil)
                    (tags "REFILE"
                          ((org-agenda-overriding-header "Tasks to Refile")
                           (org-tags-match-list-sublevels nil))))
                   nil))))

    (defun nico/org-agenda-log (arg)
      (let ((org-agenda-files org-agenda-files))
        (add-to-list 'org-agenda-files "~/Documents/configuration/org/tasks.org_archive")
        (add-to-list 'org-agenda-files "~/Documents/configuration/org/someday.org_archive")
        (org-agenda-list arg)
        (org-agenda-log-mode)
        (org-agenda-earlier 1)))

    (setq org-default-calendar-file
          "~/Documents/configuration/org/schplaf.org")

    (setq org-agenda-files
          `("~/Documents/configuration/org/refile.org"
            "~/Documents/configuration/org/tasks.org"
            "~/Documents/configuration/org/someday.org"
            "~/Documents/configuration/org/repeating.org"
            ,org-default-calendar-file))

    (setq org-refile-targets `(("~/Documents/configuration/org/tasks.org"      :maxlevel . 2)
                               ("~/Documents/configuration/org/someday.org"    :maxlevel . 2)
                               ("~/Documents/configuration/org/repeating.org"  :maxlevel . 2)))

    (setq org-todo-keywords
          '((sequence "TODO(t)"    "|" "DONE(d)" "CANCELLED(c)")))

    (setq org-capture-templates
          '(("t" "Todo" entry
             (file org-default-notes-file)
             "* TODO %?%i")
            ("s" "Schedule" entry
             (file org-default-calendar-file)
             "* %?\n%^T")))

    (unbind-key "C-'" org-mode-map)

    (add-to-list 'org-file-apps '("\\.png\\'" . default))))

(use-package ox-twbs
  :after org)

(use-package org-caldav
  :bind (("C-. o S"   . org-caldav-sync))
  :config
  (progn
    (setq org-caldav-url "https://cassou.me/baikal/cal.php/calendars/damien"
          org-caldav-calendar-id "default"
          org-caldav-inbox org-default-calendar-file
          org-caldav-files '()
          org-icalendar-timezone "Europe/Berlin"
          org-caldav-save-directory "~/.emacs.d/cache/org-caldav"
          org-caldav-sync-changes-to-org 'all)))

(use-package drag-stuff
  :demand t
  :diminish drag-stuff-mode
  :config
  (progn
    (drag-stuff-global-mode t)
    (add-to-list 'drag-stuff-except-modes 'org-mode)
    (add-to-list 'drag-stuff-except-modes 'js-mode)
    (add-to-list 'drag-stuff-except-modes 'rebase-mode)))

(use-package expand-region
  :bind ("C-x =" . er/expand-region))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package shell-switcher
  :load-path "packages/shell-switcher"
  :bind (("C-M-'"   . shell-switcher-new-shell)
         ("C-'"     . shell-switcher-switch-buffer)
         ("C-x 4 '" . shell-switcher-switch-buffer-other-window)))

(use-package guide-key
  :demand t
  :diminish guide-key-mode
  :config
  (progn
    (guide-key-mode 1)))

(use-package discover
  :demand t
  :config
  (progn
    (global-discover-mode 1)))

(use-package projectile
  :demand t
  :diminish projectile-mode
  :config
  (progn
    (projectile-global-mode)))

(use-package helm-projectile
  :demand t
  :after projectile
  :config
  (progn
    (helm-projectile-on)))

(use-package unify-opening
  :demand t
  :load-path "packages/unify-opening")

(eval-and-compile
  (setq-default notmuch-command (executable-find "notmuch")))

(use-package notmuch
  :if notmuch-command
  :load-path (lambda ()
               (and notmuch-command
                    (list (expand-file-name
                           "../../share/emacs/site-lisp"
                           (file-symlink-p notmuch-command)))))
  :bind (("C-. m" . notmuch))
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
      (bind-key "R" #'notmuch-show-reply-sender notmuch-show-mode-map))))

(use-package profile
  :after (notmuch)
  :load-path "packages/profile"
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
             (smtpmail-queue-dir . "~/Mail/Perso/queued-mail/")
             (smtpmail-local-domain . nil)
             (smtpmail-smtp-user . "damien@cassou.me")
             (smtpmail-smtp-server . "choca.pics")
             (smtpmail-stream-type . ssl)
             (smtpmail-smtp-service . 465))
            ("Ftgp"
             (profile-maildir . "/Ftgp")
             (user-mail-address . "damien.cassou@foretagsplatsen.se")
             (smtpmail-queue-dir . "~/Mail/Ftgp/queued-mail/")
             (smtpmail-local-domain . nil)
             (smtpmail-smtp-user . "damien.cassou@foretagsplatsen.se")
             (smtpmail-smtp-server . "smtp.gmail.com")
             (smtpmail-stream-type . starttls)
             (smtpmail-smtp-service . 587))))
    (profile-set-profile-from-name "Perso")
    (setq profile-extra-email-addresses
          (quote
           ("damien.cassou@lifl.fr" "cassou@inria.fr"
            "damien.cassou@laposte.net" "damien@foretagsplatsen.se")))
    (setq profile-noisy-query
          "to:\"notmuch@notmuchmail.org\" OR to:\"offlineimap-project@lists.alioth.debian.org\" OR list:\"nix-dev\" OR to:\"emacs-devel\"")

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

    (setq notmuch-saved-searches
          `((:name "inbox" :query ,(profile-inbox-query) :key "i")
            (:name "noisy" :query ,(profile-noisy-unarchived-list-query) :key "n")
            (:name "ftgp" :query "(foretagsplatsen OR ftgp) AND tag:inbox" :key "f")
            (:name "ignored" :query "tag:inbox AND (no-reply@slack.com OR to:support@foretagsplatsen.se OR (to:current@foretagsplatsen.se AND subject:FATAL) OR subject:production-ftgp OR subject:\"master Error\" OR subject:\"ben Error\" OR to:info@foretagsplatsen.se)" :key "g")
            (:name "unread" :query "tag:unread" :key "u")
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
    (add-hook 'lisp-mode-hook #'enable-paredit-mode))
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
         ("<C-mouse-5>" . zoom-out)))

(use-package visible-mark
  :demand t
  :config
  (progn
    (global-visible-mark-mode 1)))

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
              ("C-l" . find-library)))

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
         ("M-i"     . helm-imenu)
         ("C-h SPC" . helm-all-mark-rings)
         ("C-:"     . helm-eval-expression-with-eldoc)
         :map helm-map
         ("<tab>"   . helm-execute-persistent-action)
         ("C-i"     . helm-execute-persistent-action)
         ("C-z"     . helm-select-action)
         ("<S-left>"  . helm-beginning-of-buffer)
         ("<S-right>" . helm-end-of-buffer)
         :map dired-mode-map ;; helm-imenu doesn't work with dired
         ("M-i"     . imenu))
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

    (helm-mode 1)))

(use-package helm-descbinds
  :after helm
  :config
  (progn
    (helm-descbinds-mode)))

(use-package password-store
  :config
  (progn
    (setq password-store-password-length 16)))

(use-package auth-password-store
  :demand t
  :load-path "packages/auth-password-store"
  :config
  (progn
    (setq auth-sources '(password-store))))

(use-package ace-window
  :bind* (("M-o" . ace-window))
  :config
  (progn
    ;; keys under my fingers (blue keys on my Kinesis Advantage)
    (setq aw-keys '(?a ?s  ?d  ?f  ?j  ?k  ?l  59 32))))

(use-package avy
  :bind* (("C-," . avy-goto-char-2))
  ;; dont :bind* this as this would override `M-g` in Projectile
  ;; (helm-projectile-vc):
  :bind (("M-g g" . avy-goto-line))
  :config
  (progn
    ;; keys under my fingers (blue keys on my Kinesis Advantage)
    (setq avy-keys '(?a ?s  ?d  ?f  ?j  ?k  ?l  59 32))))

(use-package outline
  :diminish outline-minor-mode)

(use-package beginend
  :diminish (beginend-dired-mode beginend-message-mode)
  :load-path "packages/beginend"
  :after (dired message)
  :config
  (progn
    (beginend-setup-all)))

(use-package vdirel
  :load-path "packages/vdirel"
  :bind (("C-. c" . vdirel-helm-select-email)))

(use-package help
  :bind (:map help-mode-map
              ("g" . my:revert-buffer-no-confirm))
  :config
  (progn
    (defun my:revert-buffer-no-confirm (&optional ignore-auto)
      "Revert current buffer without asking."
      (interactive (list (not current-prefix-arg)))
      (revert-buffer ignore-auto t nil))))

(use-package nameless
  :diminish nameless-mode
  :commands (nameless-mode)
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'nameless-mode)))

(use-package beacon
  :demand t
  :diminish beacon-mode
  :config
  (progn
    ;; don't blink in notmuch-search, it's both slow and ugly
    (add-to-list 'beacon-dont-blink-major-modes #'notmuch-search-mode)
    (beacon-mode)))

(use-package subword
  :diminish subword-mode)

(use-package company
  :diminish company-mode
  :commands (company-mode)
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'company-mode)))

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
  :load-path "packages/amd-mode"
  :diminish amd-mode)

(use-package jade
  :load-path "packages/jade"
  :diminish jade-interaction-mode)

(use-package gulp-task-runner
  :commands (gulp)
  :load-path "packages/gulp-task-runner")

(use-package ftgp
  :demand t
  :load-path "packages/ftgp")

(use-package jabber
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
    (defun my/jabber-start ()
      (interactive)
      (require 'jabber)
      (setq jabber-account-list
            `(("damien@cassou.me"
               (:password . ,(password-store-get "ldn-fai.net")))))
      (add-hook 'jabber-post-connect-hooks #'jabber-autoaway-start)
      (add-hook 'jabber-chat-mode-hook #'flyspell-mode)

      ;; Override jabber.el global key
      (bind-key "C-x C-j" #'dired-jump)

      (jabber-connect-all))))

(use-package diff-hl
  :commands (diff-hl-mode)
  :init
  (progn
    (add-hook 'prog-mode-hook #'diff-hl-mode)))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-minor-mode)
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook #'yas-minor-mode))
  :config
  (progn
    (add-to-list 'yas-snippet-dirs "~/.emacs.d/packages/yasnippet-snippets")
    (yas-reload-all)))

(use-package ws-butler
  :diminish ws-butler-mode
  :commands (ws-butler-mode)
  :init
  (progn
    (add-hook 'prog-mode-hook #'ws-butler-mode)))

(use-package editorconfig
  :commands (editorconfig-mode)
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

;;; Emacs Configuration
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; eval: (outline-minor-mode)
;; eval: (flycheck-mode -1)
;; End:
