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
 '(beginend-global-mode t)
 '(bookmark-save-flag 1)
 '(calendar-date-style (quote european))
 '(calendar-week-start-day 1)
 '(checkdoc-spellcheck-documentation-flag t)
 '(column-number-mode t)
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil)
 '(confirm-kill-emacs (quote y-or-n-p))
 '(current-language-environment "UTF-8")
 '(custom-safe-themes t)
 '(debbugs-gnu-trunk-directory "~/Documents/projects/emacs/")
 '(delete-active-region nil)
 '(delete-by-moving-to-trash t)
 '(dired-dwim-target t)
 '(dired-listing-switches "-alh")
 '(dired-omit-verbose nil)
 '(dired-recursive-deletes (quote always))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(electric-pair-mode t)
 '(enable-local-variables :all)
 '(enable-recursive-minibuffers t)
 '(erc-autojoin-channels-alist (quote (("freenode.net"))))
 '(erc-nick "DamienCassou")
 '(eval-expression-print-length 20)
 '(eval-expression-print-level nil)
 '(flyspell-use-meta-tab nil)
 '(frame-title-format "Emacs: %b" t)
 '(gc-cons-threshold 20000000)
 '(global-font-lock-mode t)
 '(global-pair-mode t)
 '(global-prettify-symbols-mode nil nil nil "Affects indentation...")
 '(global-subword-mode t)
 '(google-translate-default-source-language "sv")
 '(google-translate-default-target-language "en")
 '(imenu-auto-rescan t)
 '(imenu-max-item-length 200)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(initial-major-mode (quote text-mode))
 '(initial-scratch-message nil)
 '(ispell-dictionary "english")
 '(ispell-program-name "/usr/bin/hunspell")
 '(jabber-auto-reconnect t)
 '(jabber-backlog-days 30)
 '(jabber-backlog-number 100)
 '(jabber-history-enabled t)
 '(load-prefer-newer t)
 '(magit-diff-refine-hunk (quote all))
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
 '(nameless-affect-indentation-and-filling nil)
 '(nameless-prefix "名")
 '(next-screen-context-lines 5)
 '(notmuch-always-prompt-for-sender t)
 '(notmuch-archive-tags (quote ("-inbox" "-unread")))
 '(notmuch-crypto-process-mime t)
 '(notmuch-hello-sections (quote (notmuch-hello-insert-saved-searches)))
 '(notmuch-labeler-hide-known-labels t)
 '(notmuch-search-oldest-first nil)
 '(nsm-save-host-names t)
 '(offlineimap-command "offlineimap -u machineui")
 '(org-babel-load-languages (quote ((sh . t) (emacs-lisp . t) (java . t) (python . t))))
 '(org-catch-invisible-edits (quote error))
 '(org-clock-clocked-in-display nil)
 '(org-completion-use-ido t)
 '(org-default-notes-file "~/Documents/configuration/org/refile.org")
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
 '(org-special-ctrl-a/e t)
 '(org-startup-align-all-tables t)
 '(org-table-use-standard-references nil)
 '(org-time-stamp-rounding-minutes (quote (10 10)))
 '(org-use-speed-commands t)
 '(package-archive-priorities (quote (("melpa-stable" . 10))))
 '(powerline-display-buffer-size nil)
 '(proced-filter (quote all))
 '(projectile-completion-system (quote helm))
 '(projectile-keymap-prefix (kbd "C-. p"))
 '(projectile-require-project-root nil)
 '(read-file-name-completion-ignore-case t)
 '(recentf-auto-cleanup 300)
 '(recentf-exclude (quote ("~$" "\\.log$")))
 '(recentf-max-saved-items 4000)
 '(report-emacs-bug-no-explanations t)
 '(runner-run-in-background t)
 '(safe-local-variable-values
   (quote
    ((eval add-to-list
           (quote grep-find-ignored-files)
           "archive-contents"))))
 '(save-interprogram-paste-before-kill t)
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/places")
 '(scheme-program-name "petite")
 '(scroll-bar-mode nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(sh-indent-comment t)
 '(shell-switcher-ask-before-creating-new t)
 '(shell-switcher-mode t)
 '(shell-switcher-new-shell-function (quote shell-switcher-make-shell))
 '(show-paren-mode t)
 '(skeletor-project-directory "/home/cassou/.emacs.d/packages/")
 '(skeletor-show-project-command (quote magit-status))
 '(smart-tab-completion-functions-alist nil)
 '(smart-tab-using-hippie-expand t)
 '(smtpmail-debug-info t)
 '(smtpmail-debug-verb t)
 '(smtpmail-queue-mail nil)
 '(smtpmail-stream-type (quote starttls))
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(truncate-partial-width-windows nil)
 '(undo-limit 5000000)
 '(undo-outer-limit 200000000)
 '(undo-strong-limit 10000000)
 '(undo-tree-auto-save-history t)
 '(undo-tree-mode-lighter "")
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(use-package-always-defer t)
 '(use-package-enable-imenu-support t)
 '(use-package-minimum-reported-time 0)
 '(use-package-verbose t)
 '(user-full-name "Damien Cassou")
 '(user-mail-address "damien@cassou.me")
 '(vc-follow-symlinks nil)
 '(vdirel-repository "~/Documents/configuration/contacts")
 '(version-control t)
 '(visible-bell nil)
 '(visible-mark-faces (quote (visible-mark-face1 visible-mark-face2)))
 '(visible-mark-max 2)
 '(winner-mode t nil (winner) "Use C-c <left|right> to go back to previous windows configuration")
 '(zoom-frame/buffer (quote frame)))

(add-to-list 'load-path "~/.emacs.d/packages/no-littering")
(require 'no-littering)

(setq package-selected-packages
      '(
        4clojure ; learn clojure
        ace-link ; type o in help-mode to go to a link
        ace-window ; manage windows with ace-like behavior
        ag ; search using the 'ag' command (better grep)
        aggressive-indent ; indent code automatically while typing
        all-the-icons ; library with many icons (zerodark dependency)
        anzu ; more interactive query-replace
        assess ; library to facilitate test writing
        avy ; move fast in buffer with <C-,>
        beacon ; highlight my cursor when scrolling
        bind-key ; to simplify definition of shortcuts
        camcorder ; record emacs sessions M-x camcorder-record
        company-restclient ; provides completion for restclient
        company-tern ; tern backend for company mode
        counsel ; Various completion functions using Ivy
        csharp-mode ; C# major mode
        dash ; list library
        debbugs ; SOAP library to access debbugs servers
        diff-hl ; shows git status in buffer's fringe
        diminish ; Shorter mode names in the modeline
        dired-imenu ; integrates imenu in dired
        dired-toggle-sudo ; <C-x s> to toggle sudo state of buffer
        drag-stuff ; use <M-arrow> to move things around
        duplicate-thing ; M-D to duplicate thing at point
        editorconfig ; handle .editorconfig files automatically
        embrace ; wrap/unwrap/change wrapping for quote, braces, ...
        eslintd-fix ; Automatically fix javascript with eslint_d
        exec-path-from-shell ; For indium
        expand-region ; <C-x => repeadly to mark regions
        f ; file manipulation library
        feature-mode ; major mode for editing feature files
        flycheck ; flycheck to check files on the fly
        flycheck-cask ; use Cask when present for dependencies
        flycheck-package ; checks elisp package metadata
        flycheck-nim ; checks programs written in Nim
        google-translate ; to translate current region and more
        gitattributes-mode ; major mode for editing .gitattributes files
        gitconfig-mode ; major mode for editing .gitconfig files.
        git-timemachine ; history of a file with M-x git-timemachine
        grunt ; glue for grunt files (Javascript)
        guess-language ; automatic language detection
        helm ; selection/completion interface for everything
        helm-ag ; use ag from helm
        helm-descbinds ; list available bindings through helm
        helm-flyspell ; use helm for flyspell suggestions
        helm-projectile ; integrate projectile and helm <C-. p h>
        jabber ; instant messaging
        js2-mode ; Improved Javascript editing mode
        js2-refactor ; A Javascript refactoring library
        json-mode ; Major mode to edit JSON files
        less-css-mode ; Major mode to edit .less files
        macrostep ; Interactively expand macros
        magit ; Integrate git <C-x g>
        make-it-so ; Transform files with Makefile recipes
        makey ; required by widgetjs and amd
        markdown-mode ; Major mode for markdown format
        messages-are-flowing ; Visual indication of hard newlines
        multiple-cursors ; Control multiple cursors with <C-S-c C-S-c>
        nameless ; hide current package name everywhere in elisp code
        nim-mode ; major mode for the Nim programming language
        notmuch ; email client
        ob-nim ; org babel functions for the Nim programming language
        offlineimap
        ;; I need my packages/org-caldav instead ; org ↔ caldav
        org-vcard ; used by vdirel
        orgtbl-show-header ; show header of column in minibuffer
        ox-twbs ; use twitter bootstrap to export org files to HTML
        paredit ; edit lisp AST instead of characters
        paren-face ; hide parenthesis in elisp code
        pass ; Nicolas' major mode for password-store
        password-store ; get passwords from the 'pass' command
        pos-tip ; make tool-tips appear nicely
        prodigy ; manage external services from within Emacs
        projectile ; add notion of projects
        refine ; edit list interactively
        restclient ; test REST queries
        restclient-helm ; helm interface for restclient
        runner ; Associate external applications to file extensions
        s ; string library
        skeletor ; facilitates the creation of new project
        smartscan ; <M-n> <M-p> to move between same symbol in buffer
        snapshot-timemachine-rsnapshot ; rsnapshot backend for snapshot-timemachine
        tern ; Javascript code analyzer
        undo-tree ; <C-x u> to show the undo tree
        use-package ; to structure my init.el file
        visible-mark ; show the current mark
        websocket ; dependency of jade
        which-key ; displays bindings after a short break in a key sequence
        ws-butler ; trim whitespace only in edited lines
        xref-js2 ; Jump to references/definitions using ag & js2-mode's AST (JavaScript)
        yaml-mode ; to edit *.yml files (including .travis.yml)
        yasnippet ; expand snippets of text
        zoom-frm ; change font size for all buffers <C-x C-+>
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
  :demand t
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

(use-package em-term
  :config
  (progn
    (add-to-list 'eshell-visual-commands "htop")
    (add-to-list 'eshell-visual-commands "journalctl")
    (add-to-list 'eshell-visual-commands "karma")
    (add-to-list 'eshell-visual-commands "bower")))

(use-package magit
  :load-path "~/.emacs.d/packages/magit/lisp"
  :diminish (magit-auto-revert-mode magit-wip-after-save-mode magit-wip-after-apply-mode magit-wip-affter-change)
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-dispatch-popup))
  :config
  (progn
    (global-magit-file-mode)
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
          (quote ((" " "Agenda"
                   ((agenda "" nil)
                    (tags "REFILE"
                          ((org-agenda-overriding-header "Tasks to Refile")
                           (org-tags-match-list-sublevels nil))))
                   nil))))

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

(use-package org-notmuch
  :load-path "lisp"
  :after org)

(use-package ox-twbs
  :after org)

(use-package org-caldav
  :load-path "packages/org-caldav"
  :bind (("C-. o S"   . org-caldav-sync))
  :config
  (progn
    (setq org-caldav-url "https://cassou.me/baikal/cal.php/calendars/damien"
          org-caldav-calendar-id "default"
          org-caldav-inbox org-default-calendar-file
          org-caldav-files '()
          org-icalendar-timezone "Europe/Berlin"
          org-caldav-sync-changes-to-org 'all)))

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
  :load-path "packages/shell-switcher"
  :bind (("C-M-'"   . shell-switcher-new-shell)
         ("C-'"     . shell-switcher-switch-buffer)
         ("C-x 4 '" . shell-switcher-switch-buffer-other-window)))

(use-package which-key
  :demand t
  :diminish which-key-mode
  :config
  (progn
    (which-key-mode)))

(use-package projectile
  :demand t
  :diminish projectile-mode
  :config
  (progn
    (projectile-global-mode)))

(use-package helm-projectile
  :load-path "packages/helm-projectile"
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
  :bind (("C-. m" . notmuch)
         ("C-. M" . notmuch-mua-new-mail))
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
          `((:name "inbox" :query ,(format "(folder:\"Perso/INBOX\") AND (NOT (%s) OR recip:damien*)"
                                           profile-noisy-query) :key "i")
            (:name "noisy" :query ,(profile-noisy-unarchived-list-query) :key "n")
            (:name "ftgp" :query "folder:\"Ftgp/INBOX\" AND tag:inbox" :key "f")
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
  :demand t
  :diminish beginend-global-mode
  :load-path "packages/beginend")

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
  :bind ("C-. /" . company-complete)
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

(use-package indium
  :load-path "packages/indium"
  :diminish indium-interaction-mode)

(use-package gulp-task-runner
  :commands (gulp)
  :load-path "packages/gulp-task-runner")

(use-package ftgp
  :demand t
  :load-path "packages/ftgp"
  :config
  (progn
    (require 'bookmark)
    (setq ftgp-monitor-root-location
          (expand-file-name (bookmark-location "ftgp-monitor-root")))))

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
  :config
  (progn
    (defun my/offlineimap-message-when-done (message-type &optional action)
      (if (string-match "^finished" message-type)
          (message "Offlineimap finished")
        (message "Offlineimap in progress")))
    (add-hook 'offlineimap-event-hooks #'my/offlineimap-message-when-done)))

(use-package google-translate)

(use-package ob-nim
  :after org
  :config
  (progn
    (add-to-list 'org-babel-load-languages (cons 'nim t))))

(use-package guess-language
  :disabled t
  :diminish guess-language-mode
  :init
  (progn
    (add-hook 'text-mode-hook #'guess-language-mode))
  :config
  (progn
    (setq guess-language-languages '(en fr))
    (setq guess-language-langcodes
          '((en . ("en_US" "English"))
            (fr . ("francais" "French"))))))

(use-package helm-flyspell
  :bind (("M-$" . helm-flyspell-correct)))

(use-package my-misc
  :demand t
  :load-path "lisp")

(use-package messages-are-flowing
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
  :load-path "~/.emacs.d/packages/skeletor"
  :commands (skeletor-create-project))

(use-package buttercup
  :load-path "~/.emacs.d/packages/buttercup")

(use-package hierarchy
  :load-path "~/.emacs.d/packages/hierarchy")

(use-package klassified
  :load-path "~/.emacs.d/packages/klassified"
  :commands (klassified-interaction-js-mode)
  :init
  (progn
    (add-hook 'js-mode-hook #'klassified-interaction-js-mode)))

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
