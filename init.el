;;; init.el --- user-init-file                    -*- lexical-binding: t; -*-


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(setq package-enable-at-startup nil)

(setq load-prefer-newer t)

(progn ;    `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require  'borg)
  (borg-initialize))

(progn ; `use-package'
  (setq use-package-always-defer t)
  (setq use-package-enable-imenu-support t)
  (setq use-package-minimum-reported-time 0)
  (setq use-package-verbose t)
  (setq use-package-compute-statistics t)
  (require 'use-package))

(use-package auto-compile
  :demand t
  :init
  (progn
    (setq auto-compile-display-buffer nil)
    (setq auto-compile-mode-line-counter t)
    (setq auto-compile-source-recreate-deletes-dest t)
    (setq auto-compile-toggle-deletes-nonlib-dest t)
    (setq auto-compile-update-autoloads t))
  :hook (auto-compile-inhibit-compile . auto-compile-inhibit-compile-detached-git-head)
  :config
  (progn
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)))

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
  (setq version-control 'never))

(progn ; `window'
  (bind-key "C-;" #'other-window)
  (unbind-key "C-x o")

  (defun my/swap-last-buffers ()
    "Replace currently visible buffer by last one."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer))))

  (bind-key "C-x B" #'my/swap-last-buffers)

  (defun my/kill-this-buffer ()
    "Kill current buffer.
Better version of `kill-this-buffer' whose docstring says it is
unreliable."
    (interactive)
    (kill-buffer (current-buffer)))

  (bind-key "C-x k" #'my/kill-this-buffer)

  (defun my/toggle-window-split ()
    "Swap between horizontal and vertical separation when 2 frames
are visible."
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))

  (define-key ctl-x-4-map "t" #'my/toggle-window-split))

(progn ; `eval'
  (setq debugger-stack-frame-as-list t)
  (setq debug-on-error t))

(progn ; `map-ynp'
  ;; Make all "yes or no" prompts show "y or n" instead
  (setq read-answer-short t)
  (fset 'yes-or-no-p 'y-or-n-p))

(progn ; `editfns'
  (put 'narrow-to-region 'disabled nil))

(use-package custom
  :demand t
  :config
  (progn
    (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
    (when (file-exists-p custom-file)
      (load custom-file))))

(use-package frame
  :bind (("C-x C-z" . my/suspend-on-tty-only))
  :config
  (progn
    (defun my/suspend-on-tty-only ()
      "Suspend Emacs, but only if in tty mode."
      (interactive)
      (unless window-system
        (suspend-frame)))

    (defun my/set-selected-frame-dark ()
      "Make current frame use GTK dark theme."
      (interactive)
      (let ((frame-name (cdr (assq 'name (frame-parameters (selected-frame))))))
        (call-process-shell-command
         (format
          "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT 'dark' -name '%s'"
          frame-name))))

    (defun my/setup-frame (&optional frame)
      "Configure look of FRAME.

If FRAME is nil, configure current frame. If non-nil, make FRAME
current."
      (when frame (select-frame frame))
      (setq frame-title-format '(buffer-file-name "%f" ("%b")))
      (when (window-system)
        (ignore-errors
          (load-theme 'zerodark t)
          (setq zerodark-theme-display-vc-status 'full)
          (setq zerodark-modeline-vc
                '(vc-mode ("   "
                           (:eval (all-the-icons-faicon "code-fork"
                                                        :height 0.9
                                                        :v-adjust 0
                                                        :face (when (zerodark--active-window-p)
                                                                (zerodark-git-face))))
                           (:eval (propertize (truncate-string-to-width vc-mode 15 nil nil "…")
                                              'face (when (zerodark--active-window-p)
                                                      (zerodark-git-face)))))))
          (zerodark-setup-modeline-format))
        (my/set-selected-frame-dark)
        (set-face-attribute 'default nil :height 125 :family "Fira Mono")))

    (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

    (if (daemonp)
        (add-hook 'after-make-frame-functions #'my/setup-frame)
      (my/setup-frame))))

(use-package simple
  :demand t
  :bind (("M-j" . my/join-line)
         ;; Replace `just-one-space' by the more advanced `cycle-spacing'.
         ("M-SPC" . cycle-spacing)
         ("<S-left>" . beginning-of-buffer)
         ("<S-right>" . end-of-buffer)
         :map process-menu-mode-map
         ("k" . process-menu-delete-process))
  :init
  (progn
    (setq delete-active-region nil)
    (setq eval-expression-print-length 20)
    (setq eval-expression-print-level nil))
  :config
  (progn
    (defun my/join-line ()
      "Join current line and the next."
      (interactive)
      (join-line -1))

    (column-number-mode)))

(use-package epkg
  :init
  (progn
    (setq epkg-repository
          (no-littering-expand-var-file-name "epkgs"))))

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

(use-package time
  :init
  (progn
    (setq display-time-24hr-format t)))

(use-package time-stamp
  :init (progn
          (defvar-local time-stamp-target nil
            "File in which time-stamps should be written.")

          (put 'time-stamp-target 'safe-local-variable 'string-or-null-p)

          (defun time-stamp-target ()
            "Update the time-stamp in `time-stamp-target' if non-nil."
            (when (and time-stamp-target
                       (file-exists-p time-stamp-target))
              (with-current-buffer (find-file-noselect time-stamp-target)
                (time-stamp))))))

(use-package nsm ;; network security
  :init
  (progn
    (setq network-security-level 'high)
    (setq nsm-save-host-names t)))

(use-package imenu
  :init
  (progn
    (setq imenu-auto-rescan t)
    (setq imenu-max-item-length 200)))

(use-package url-vars
  :init
  (progn
    (setq url-privacy-level 'high)))

(use-package autorevert
  :demand t
  :hook (dired-mode . auto-revert-mode)
  :init
  (progn
    (setq auto-revert-verbose nil))
  :config
  (progn
    (global-auto-revert-mode)))

(use-package package
  :after package-lint
  :config
  (progn
    ;; the following is useful for package-lint
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
    (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
    (package-initialize)
    (unless (file-directory-p package-user-dir)
      ;; only contact elpa repositories if we don't have anything yet
      (package-refresh-contents))))

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

(use-package bug-reference
  :bind ((
          :map bug-reference-map
          ("C-c C-o" . bug-reference-push-button))))

(use-package smime
  :config
  ;; https://src.fedoraproject.org/rpms/emacs/blob/f27/f/default.el
  (setq smime-CA-directory "/etc/ssl/certs"))

(use-package abbrev
  :init
  (progn
    (setq-default abbrev-mode t)))

(use-package facemenu
  :config
  (progn
    (unbind-key "M-o")))

(use-package display-line-numbers
  :hook ((prog-mode text-mode) . my/display-line-numbers)
  :config
  (progn
    (defun my/display-line-numbers ()
      (when buffer-file-name
        (display-line-numbers-mode)))))

(use-package hl-line
  :hook ((ledger-report-mode tabulated-list-mode compilation-mode org-agenda-mode)
         .
         hl-line-mode))

(use-package undo-tree
  :demand t
  :config
  (progn
    (global-undo-tree-mode)
    (define-key undo-tree-map (kbd "C-x r") nil)))

(use-package dired
  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         ("C-a" . my/dired-move-beginning-of-line)
         ("k" . dired-do-delete))
  :hook (dired-mode . dired-hide-details-mode)
  :init
  (progn
    (setq dired-dwim-target t)
    (setq dired-listing-switches "-alh")
    (setq dired-recursive-deletes 'always)

    (defun my/dired-move-beginning-of-line ()
      (interactive)
      (let ((point (point)))
        (dired-move-to-filename)
        (when (= point (point))
          (move-beginning-of-line nil))))))

(use-package gnus-dired
  :hook (dired-mode . turn-on-gnus-dired-mode))

(use-package runner
  :demand t
  :after dired
  :init
  (progn
    (setq runner-run-in-background t)))

(use-package dired-x
  :after dired
  :bind (:map dired-mode-map
              (")" . dired-omit-mode))
  :hook (dired-mode . dired-omit-mode)
  :init
  (progn
    (setq dired-omit-verbose nil))
  :config
  (progn
    (setq dired-omit-files (rx-to-string `(or (and bol ".tern-port" eol)
                                              (regexp ,dired-omit-files))))))

(use-package dired-imenu
  :demand t
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
    (recentf-mode)))

(use-package magit
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
    (setq magit-branch-adjust-remote-upstream-alist '(("origin/master" "master")))
    (setq magit-branch-arguments nil)
    (setq magit-module-sections-nested nil)
    (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
    (setq magit-no-confirm '(amend-published))
    (setq magit-revision-insert-related-refs nil)
    (setq magit-revision-show-gravatars t))
  :config
  (progn
    ;; Enable magit-clean
    (put 'magit-clean 'disabled nil)
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-modules
                            'magit-insert-unpulled-from-upstream)

    ;; Only show the module sections I'm interested in
    (with-eval-after-load "magit-submodule"
      (remove-hook 'magit-module-sections-hook 'magit-insert-modules-overview)
      (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpulled-from-pushremote)
      (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-upstream)
      (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-pushremote))))

(use-package magit-tbdiff
  :demand t
  :after magit)

(use-package forge
  :demand t
  :after magit
  :config
  (progn
    (add-to-list 'forge-alist
                 '("gitlab.petton.fr"
                   "gitlab.petton.fr/api/v4"
                   "gitlab.petton.fr"
                   forge-gitlab-repository))))

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
  :after (:any elbank helpful info)
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
    (setq ispell-program-name (executable-find "hunspell"))

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
  :bind (("C-. f b" . flyspell-buffer))
  :hook (text-mode . flyspell-mode)
  :init
  (progn
    (setq flyspell-use-meta-tab nil))
  :config
  (progn
    (unbind-key "C-." flyspell-mode-map)
    (unbind-key "C-;" flyspell-mode-map)))

(use-package flyspell-correct-ivy
  :bind (("M-$" . flyspell-correct-at-point)))

(use-package checkdoc
  :init
  (progn
    (setq checkdoc-spellcheck-documentation-flag t)))

(use-package flycheck
  :init
  (progn
    (setq flycheck-emacs-lisp-check-declare t)
    (setq flycheck-emacs-lisp-load-path 'inherit)))

(use-package flycheck-ledger
  :after (flycheck ledger-mode)
  :demand t
  :init
  (progn
    (setq flycheck-ledger-pedantic 'check-payees)
    (setq flycheck-ledger-explicit t)
    (setq flycheck-ledger-zero-accounts
          '("^Assets:Budget:Available"
            "^Assets:Budget:Taxes:Source"
            "^Assets:Budget:Unbudgeted"))))

(use-package flycheck-package
  :demand t
  :after flycheck
  :config
  (progn
    (flycheck-package-setup)))

(use-package flycheck-elsa
  :init
  (progn
    (setq flycheck-elsa-command 'host)))

(use-package ledger-mode
  :hook (ledger-mode . my/configure-ledger-mode)
  :init
  (progn
    (setq ledger-reports
          (mapcar
           (lambda (pair)
             (list (car pair)
                   (format "%s %s"
                           "%(binary) -f %(ledger-file)"
                           (cdr pair))))
           '(("AOM days"          . "register --real --related --invert --sort -date ^Expenses")
             ("Cash Flow"         . "register --real --related --invert --period %(month) ^Assets:Current")
             ("Monthly cash flow" . "register --real --monthly --collapse ^Assets:Current")
             ("Monthly balance"   . "register --real --monthly --collapse ^Assets ^Liabilities ^Equity")
             ("Account statement" . "register ^%(account)")
             ("Checks"            . "register --group-by=payee --payee=code --sort=payee --uncleared :Check")

             ("Income statement"  . "balance --real --period %(month) --invert --sort T ^Income ^Expenses")
             ("Balance sheet"     . "balance --real ^Assets ^Liabilities ^Equity")
             ("Budget"            . "balance --empty --sort account ^Assets:Budget and not \\(Available or Unbudgeted\\)")
             ("Check Budget"      . "balance --depth 1 ^Assets ^Liabilities ^Equity:Budget")
             ("WK expenses"       . "register --effective --begin 2019-05 --end 2019-06 --collapse ^Assets:Receivables:WK")

             ("Equity"            . "equity --real"))))

    (setq ledger-reconcile-default-commodity "EUR")
    (setq ledger-report-links-in-register t)
    (setq ledger-report-use-header-line t)
    (setq ledger-report-use-native-highlighting t)
    (setq ledger-report-auto-refresh-sticky-cursor t)
    (setq ledger-report-use-strict t)

    (defvar my/ledger-rate-history (list)
      "Keeps track of entered SEK/EUR rates.")

    (defun my/ledger-insert-sek-eur (sek rate)
      "Insert amount in eur corresponding to SEK * RATE."
      (interactive
       (list (string-to-number (read-string "Amount in SEK: "))
             (string-to-number (read-string "EUR/SEK Rate: " nil 'my/ledger-rate-history my/ledger-rate-history))))
      (if (> rate 1)
          (my/ledger-insert-sek-eur sek (/ 1.0 rate))
        (insert
         (format
          "%.2f EUR\n    ; %s SEK @ %.5f EUR\n"
          (* sek rate)
          (abs sek)
          rate))))

    (defun my/ledger-clean-buffer ()
      "Same as `ledger-mode-clean-buffer' but limited to transactions.

Only transactions are ordered and formatted.

This is useful because the beginning of my buffer is formatted by
hand."
      (interactive)
      (let ((transactions-start (save-excursion
                                  (goto-char (point-min))
                                  (re-search-forward "^; \\* Transactions$")
                                  (line-beginning-position)))
            (transactions-end (point-max)))
        (save-restriction
          (narrow-to-region transactions-start transactions-end)
          (ledger-mode-clean-buffer)
          (whitespace-cleanup))))

    (defun my/configure-ledger-mode ()
      "Configure the current Ledger buffer."
      ;; use TAB to complete:
      (setq-local tab-always-indent 'complete)
      ;; use minibuffer completion with ivy
      (setq-local ivy-display-functions-alist nil))))

(use-package ledger-complete
  :init
  (progn
    (setq ledger-complete-in-steps nil)))

(use-package ledger-import
  :hook ((ledger-import-finished . my/ledger-import-alert))
  :init
  (progn
    (setq ledger-import-boobank-import-from-date "2019-09-15")
    (setq ledger-import-autosync-command
          '("ledger-autosync" "--assertions"
            "--payee-format" "{payee}"))


    (defun my/ledger-import-alert ()
      "Notify the user that import is finished."
      (alert "Finished"
             :title "Ledger-autosync"
             :buffer (current-buffer))))
  :config
  (progn
    ;; Fill `ledger-import-accounts' and `ledger-import-ofx-rewrite-rules':
    (let ((file (expand-file-name "~/.password-store/Secure_Notes/ledger-accounts.gpg")))
      (when (file-exists-p file)
        (load file t)))))

(use-package org
  :bind
  (("C-. o a"   . org-agenda)
   ("C-. o l"   . org-store-link)
   ("C-. o w"   . my:org-move-to-refile-target)
   ("C-. o s"   . org-save-all-org-buffers)
   ("C-. o t"   . org-capture))
  :init
  (progn
    (setq org-babel-load-languages '((shell . t) (emacs-lisp . t) (dot . t)))
    (setq org-catch-invisible-edits 'show-and-error)
    (setq org-cycle-separator-lines 0)
    (setq org-clock-clocked-in-display nil)

    (setq org-directory "~/Documents/configuration/org")
    (setq org-default-notes-file (expand-file-name "inbox.org" org-directory))
    (setq org-default-calendar-file (expand-file-name "schplaf.org" org-directory))
    (setq org-default-gtd-file (expand-file-name "gtd.org" org-directory))
    (setq org-default-someday-file (expand-file-name "someday.org" org-directory))
    (setq org-default-tickler-file (expand-file-name "tickler.org" org-directory))
    (setq org-agenda-files `(,org-default-notes-file
                             ,org-default-calendar-file
                             ,org-default-gtd-file
                             ,org-default-tickler-file))

    (setq org-refile-targets `((,org-default-notes-file :level . 1)
                               (,org-default-gtd-file :maxlevel . 3)
                               (,org-default-someday-file :level . 1)
                               (,org-default-tickler-file :maxlevel . 2)))

    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline org-default-notes-file "Inbox") "* TODO %?%i")
            ("l" "Todo + link" entry (file+headline org-default-notes-file "Inbox") "* TODO %? %a")
            ("p" "Appt" entry (file org-default-calendar-file) "* %?\n%^T")
            ("T" "Tickler" entry (file+headline org-default-tickler-file "Tickler") "* %i%? \nSCHEDULED: %^t")))

    (setq org-todo-keywords
          '((sequence "TODO(t)" "|" "DONE(d)" "CANCELLED(c)")
            (sequence "WAITING(w)" "|" "DONE(d)")))

    (defun my/org-agenda-skip-all-siblings-but-first ()
      (let (should-skip-entry)
        (unless (my/org-current-is-todo)
          (setq should-skip-entry t))
        (save-excursion
          (while (and (not should-skip-entry) (org-goto-sibling t))
            (when (my/org-current-is-todo)
              (setq should-skip-entry t))))
        (when should-skip-entry
          (or (outline-next-heading)
              (goto-char (point-max))))))

    (defun my/org-current-is-todo ()
      (string= "TODO" (org-get-todo-state)))

    (defun org-agenda-format-parent (n)
      (save-excursion
        (save-restriction
          (widen)
          (org-up-heading-safe)
          (s-truncate n (org-get-heading t t)))))

    (setq org-agenda-custom-commands
          '(("a" "Agenda for the current week" ((agenda "" nil)) nil nil)
            ("w" . "TODOs")
            ("d" "30 days deadlines" agenda ""
             ((org-agenda-entry-types '(:deadline))
              (org-agenda-overriding-header "Month deadlines")
              (org-agenda-span 'month)
              (org-agenda-overriding-header "")))
            ("n" "Next actions"
             ((alltodo ""
		       ((org-agenda-overriding-header "Next actions")
		        (org-agenda-category-filter-preset '("+projects"))
		        (org-agenda-skip-function #'my/org-agenda-skip-all-siblings-but-first)
		        (org-agenda-prefix-format "%-32:(org-agenda-format-parent 30)")
		        (org-agenda-todo-keyword-format "%-4s")
		        (org-agenda-files (list org-default-gtd-file)))))
             nil nil)
            ("@" "Contexts"
             ((tags "ftgp"
	            ((org-agenda-skip-function #'my/org-agenda-skip-all-siblings-but-first)
	             (org-agenda-overriding-header "FTGP next actions")
                     (org-agenda-prefix-format "%-32:(org-agenda-format-parent 30)")))
              (tags "emacs"
	            ((org-agenda-overriding-header "Emacs next actions")
	             (org-agenda-skip-function #'my/org-agenda-skip-all-siblings-but-first)
                     (org-agenda-prefix-format "%-32:(org-agenda-format-parent 30)")))
              (todo "WAITING"
	            ((org-agenda-overriding-header "Waiting")
                     (org-agenda-prefix-format "%-32:(org-agenda-format-parent 30)")))
              (tags-todo "@work"
		         ((org-agenda-overriding-header "At work")
		          (org-agenda-skip-function #'my/org-agenda-skip-all-siblings-but-first)
                          (org-agenda-prefix-format "%-32:(org-agenda-format-parent 30)")))
              (tags-todo "@stockholm"
		         ((org-agenda-overriding-header "At Stockholm")
		          (org-agenda-skip-function #'my/org-agenda-skip-all-siblings-but-first)
                          (org-agenda-prefix-format "%-32:(org-agenda-format-parent 30)")))
              (tags-todo "@home"
		         ((org-agenda-overriding-header "At Home")
		          (org-agenda-skip-function #'my/org-agenda-skip-all-siblings-but-first)
                          (org-agenda-prefix-format "%-32:(org-agenda-format-parent 30)"))))
             nil nil)))

    (setq org-agenda-show-future-repeats nil)
    (setq org-enforce-todo-dependencies t)
    (setq org-enforce-todo-checkbox-dependencies t)
    (setq org-ellipsis "…")
    (setq org-export-allow-bind-keywords nil)
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
    (setq org-modules '(org-protocol org-capture ox-beamer org-info))

    (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

    (defun my:org-move-to-refile-target (&optional last)
      (interactive "p")
      (require 'org)
      (org-refile (if (= last 4) '(16) '(4))))

    ;; This is my `shell-switcher-switch-buffer':
    (unbind-key "C-'" org-mode-map)

    ;; Those are my `beginning-of-buffer' and `end-of-buffer':
    (unbind-key "<S-left>" org-mode-map)
    (unbind-key "<S-right>" org-mode-map)

    (add-to-list 'org-file-apps '("\\.png\\'" . default))))

(use-package org-agenda
  :bind (
         :map org-agenda-mode-map
         ("k"         . org-agenda-kill))
  :config
  (progn
    ;; Those are my `beginning-of-buffer' and `end-of-buffer':
    (unbind-key "<S-left>" org-agenda-mode-map)
    (unbind-key "<S-right>" org-agenda-mode-map)

    (defun my/org-agenda-to-appt ()
      (interactive)
      (let ((org-agenda-files (list org-default-calendar-file)))
        (org-agenda-to-appt t)))))

(use-package org-notmuch
  :demand t
  :after (:any org notmuch))

(use-package ox-twbs
  :demand t
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
    (defvar my/french-holiday
      '((holiday-fixed 1 1 "Jour de l'an")
        (holiday-fixed 5 1 "Fête du travail")
        (holiday-fixed 5 8 "Victoire 45")
        (holiday-fixed 7 14 "Fête nationale")
        (holiday-fixed 8 15 "Assomption")
        (holiday-fixed 11 1 "Toussaint")
        (holiday-fixed 11 11 "Armistice 18")
        (holiday-fixed 12 25 "Noël")
        (holiday-easter-etc 1 "Lundi de Pâques")
        (holiday-easter-etc 39 "Ascension")
        (holiday-easter-etc 50 "Lundi de Pentecôte")))
    (setq calendar-date-style 'european)
    (setq calendar-week-start-day 1)
    (setq calendar-holidays my/french-holiday)
    (setq calendar-mark-holidays-flag t)))

(use-package drag-stuff
  :demand t
  :config
  (progn
    (drag-stuff-global-mode t)
    (drag-stuff-define-keys)
    (dolist (mode '(org-mode rebase-mode emacs-lisp-mode mpdel-playlist-mode js2-mode))
      (add-to-list 'drag-stuff-except-modes mode))))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

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
  :config
  (progn
    (which-key-mode)))

(use-package projectile
  :demand t
  :init
  (progn
    (setq projectile-completion-system 'ivy)
    (setq projectile-keymap-prefix (kbd "C-. p"))
    (setq projectile-require-project-root nil))
  :config
  (progn
    (projectile-mode)))

(use-package ivy-taskrunner
  :disabled t
  :init
  (progn
    (ivy-taskrunner-minor-mode)))

(use-package dumb-jump
  :bind (
         :map dumb-jump-mode-map
         ("M-." . dumb-jump-go))
  :init
  (progn
    (setq dumb-jump-selector 'ivy)
    (dumb-jump-mode)))

(use-package unify-opening
  :demand t)

(use-package notmuch
  :preface (setq-default notmuch-command (executable-find "notmuch"))
  :if notmuch-command
  :bind (("C-. m" . notmuch)
         :map notmuch-search-mode-map
         ;; bind 'r' to reply-all, and 'R' to reply
         ("r" . notmuch-search-reply-to-thread)
         ("R" . notmuch-search-reply-to-thread-sender))
  :init
  (progn
    (setq notmuch-always-prompt-for-sender t)
    (setq notmuch-archive-tags '("-inbox" "-unread"))
    (setq notmuch-crypto-process-mime t)
    (setq notmuch-hello-sections '(notmuch-hello-insert-saved-searches))
    (setq notmuch-labeler-hide-known-labels t)
    (setq notmuch-search-oldest-first nil)
    (setq notmuch-draft-save-plaintext t)))

(use-package notmuch-show
  :bind (
         :map notmuch-show-mode-map
         ;; bind 'r' to reply-all, and 'R' to reply
         ("r" . notmuch-show-reply)
         ("R" . notmuch-show-reply-sender)
         :map notmuch-show-part-map
         ("d" . my/notmuch-show-ics-to-org-part)
         ("a" . nico-notmuch-git-am-patch))
  :init
  (progn
    (setq notmuch-show-imenu-indent t)
    (setq notmuch-message-headers '("To" "Cc" "Subject" "Date"))

    (defun nico-notmuch-git-am-part (handle)
      (let ((dir (read-directory-name "Git directory: ")))
        (mm-pipe-part handle (format "cd %s; git am" (expand-file-name dir)))))

    (defun nico-notmuch-git-am-patch ()
      "Apply the MIME part at point as a git patch using `git am'."
      (interactive)
      (notmuch-show-apply-to-current-part-handle #'nico-notmuch-git-am-part))

    (defun my/mm-ics-to-org-part (handle &optional prompt)
      "Add message part HANDLE to org."
      (ignore prompt)
      (mm-with-unibyte-buffer
        (mm-insert-part handle)
        (mm-add-meta-html-tag handle)
        (require 'org-caldav)
        (org-caldav-import-ics-buffer-to-org)))

    (defun my/notmuch-show-ics-to-org-part ()
      "Save the .ics MIME part containing point to an org file."
      (interactive)
      (notmuch-show-apply-to-current-part-handle #'my/mm-ics-to-org-part))))

(use-package shr
  :bind ((
          :map shr-map
          ("C-c C-o" . shr-browse-url)
          :map shr-image-map
          ("C-c C-o" . shr-browse-url))))

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

(use-package profile
  :demand t
  :after notmuch
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
            ;; ("Ftgp"
            ;;  (profile-maildir . "/Ftgp")
            ;;  (notmuch-fcc-dirs . "Ftgp/Sent")
            ;;  (user-mail-address . "damien.cassou@foretagsplatsen.se")
            ;;  (message-signature . "Damien Cassou\nFöretagsplatsen AB\nPhone/Fax: +46 (0)8 774 63 00\nMobile: +33 (0)6 80 50 18 91\nAddress: Skeppsbron 26, 4tr, SE-111 30 Stockholm\nWeb: www.foretagsplatsen.se\n")
            ;;  (smtpmail-queue-dir . "~/Mail/Ftgp/queued-mail/")
            ;;  (smtpmail-local-domain . nil)
            ;;  (smtpmail-smtp-user . "damien.cassou@foretagsplatsen.se")
            ;;  (smtpmail-smtp-server . "smtp.office365.com")
            ;;  (smtpmail-stream-type . starttls)
            ;;  (smtpmail-smtp-service . 587))
            ("WK"
             (profile-maildir . "/WK")
             (notmuch-fcc-dirs . "WK/Sent")
             (user-mail-address . "damien.cassou@wolterskluwer.com")
             (message-signature . "Damien Cassou\nFinsit – a part of Wolters Kluwer Group\nPhone/Fax: +46 (0)8 774 63 00\nMobile: +33 (0)6 80 50 18 91\nAddress: Lindhagensgatan 126, 112 51 Stockholm\nWeb: www.foretagsplatsen.se\n")
             (smtpmail-queue-dir . "~/Mail/WK/queued-mail/")
             (smtpmail-local-domain . nil)
             (smtpmail-smtp-user . "damien.cassou@wolterskluwer.com")
             (smtpmail-smtp-server . "localhost")
             (smtpmail-stream-type . plain)
             (smtpmail-smtp-service . 10025))))
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
            (:name "wk" :query "folder:\"WK/INBOX\" AND tag:inbox" :key "w")
            (:name "sent" :query ,(profile-sent-query) :key "s")))

    (add-to-list 'notmuch-hello-sections
                 #'profile-queue-insert-section
                 t)))

(use-package message
  :init
  (progn
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
      (ignore arg)
      (require 'profile)
      (profile-set-profile-from-message-from-field))

    (advice-add #'message-send-and-exit
                :before
                #'my:message-send-and-exit)

    (defun my/can-encrypt-message-p ()
      "Return non-nil if current message can be encrypted.
I.e., the keyring has a public key for each recipient."
      (let ((recipients (seq-map #'cadr
                                 (seq-mapcat (lambda (header)
                                               (let ((header-value (message-fetch-field header)))
                                                 (and
                                                  header-value
                                                  (mail-extract-address-components header-value t))))
                                             '("To" "CC" "BCC"))))
            (context (epg-make-context epa-protocol)))
        (seq-every-p (lambda (recipient)
                       (not (seq-empty-p (epg-list-keys context recipient))))
                     recipients)))

    (defun my/add-encryption-mark-if-possible ()
      "Add MML tag to encrypt message when there is a key for each recipient."
      (when (my/can-encrypt-message-p)
        (mml-secure-message-sign-encrypt)))

    (add-hook 'message-send-hook #'my/add-encryption-mark-if-possible)))

(use-package elfeed
  :disabled t
  :init
  (progn
    (setq elfeed-feeds '("https://emacs.stackexchange.com/feeds"))
    (setq elfeed-sort-order 'ascending)))

(use-package image
  :config
  (progn
    (imagemagick-register-types)))

(use-package paredit
  :hook ((emacs-lisp-mode lisp-mode eval-expression-minibuffer-setup scheme-mode) . enable-paredit-mode)
  :config
  (progn
    (with-eval-after-load "eldoc"
      (eldoc-add-command #'paredit-backward-delete #'paredit-close-round))))

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
  :bind (
         :map help-map
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
  :bind (("C-h k" . helpful-key))
  :init
  (progn
    (with-eval-after-load "counsel"
      (setq counsel-describe-function-function #'helpful-callable)
      (setq counsel-describe-variable-function #'helpful-variable))))

(use-package aggressive-indent
  :hook ((lisp-mode emacs-lisp-mode) . aggressive-indent-mode))

(use-package bookmark
  :init
  (progn
    (customize-save-variable 'bookmark-save-flag 1)))

(use-package counsel
  :demand t
  :bind (("M-i" . counsel-imenu)
         ("C-x 8 RET" . counsel-unicode-char)
         ("s-!" . counsel-linux-app)
         :map counsel-find-file-map
         ("C-l" . counsel-up-directory)
         ("<right>" . counsel-down-directory)
         ("<left>" . counsel-up-directory))
  :init
  (progn
    (setq counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
    (setq counsel-yank-pop-preselect-last t))
  :config
  (progn
    (defun my/counsel-open-in-external-terminal (file)
      "Open FILE in external terminal."
      (interactive "FFile: ")
      (let ((default-directory (if (file-directory-p file)
                                   file
                                 (file-name-directory file))))
        (call-process "xterm")))

    (defun my/counsel-open-in-eshell (file)
      "Open FILE in eshell."
      (interactive "FFile: ")
      (let ((default-directory (if (file-directory-p file)
                                   file
                                 (file-name-directory file))))
        (shell-switcher-new-shell)))

    (ivy-add-actions
     'counsel-find-file
     '(("t" my/counsel-open-in-external-terminal "open terminal")
       ("s" my/counsel-open-in-eshell "eshell")))

    (defun my/apply-bookmark-fn (fn)
      "Return a function applyinig FN to a bookmark's location."
      (lambda (bookmark)
        (funcall fn (bookmark-location bookmark))))

    (ivy-add-actions
     'counsel-bookmark
     `(("t" ,(my/apply-bookmark-fn #'my/counsel-open-in-external-terminal) "open terminal")
       ("s" ,(my/apply-bookmark-fn #'my/counsel-open-in-eshell) "eshell")))

    (counsel-mode)))

(use-package ivy
  :demand t
  :bind (("C-. i" . ivy-resume)
         :map ivy-minibuffer-map
         ("M-'" . ivy-avy))
  :init
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq ivy-virtual-abbreviate 'abbreviate)
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-use-selectable-prompt t))
  :config
  (progn
    (ivy-mode)))

(use-package ivy-hydra
  :config
  (progn
    ;; deactivate ivy-hydra if it ever gets activated:
    (setq ivy-read-action-function #'ivy-read-action-by-key)))

(use-package counsel-projectile
  :demand t
  :after projectile
  :config
  (progn
    (counsel-projectile-mode)

    (counsel-projectile-modify-action
     'counsel-projectile-switch-project-action
     '((default counsel-projectile-switch-project-action-vc)))))

(use-package swiper
  :bind (("C-s" . swiper-isearch)
         :map swiper-map
         ("M-'" . swiper-avy))
  :init
  (progn
    ;; To help me stop using it
    (unbind-key "C-r")))

(use-package password-store
  :init
  (progn
    (defun my/password-length ()
      "Return a random number suitable for a password length."
      (+ 30 (random 10))))
  :config
  (progn
    (setq password-store-password-length (my/password-length))))

(use-package pass
  :commands pass
  :config
  (progn
    (defun my/pass-insert-generated (entry)
      "Same as pass-insert-generated but with my own template."
      (interactive (list (read-string "Password entry: ")))
      (when (or (not (seq-contains (password-store-list) entry))
                (yes-or-no-p "Erase existing entry with same name? "))
        (let ((password (shell-command-to-string
                         (format "pwgen --secure %s"
                                 (my/password-length)))))
          (password-store-insert
           entry
           (format "%s--\nusername: %s\nurl: https://%s\n"
                   password
                   user-mail-address
                   entry))
          (password-store-edit entry)
          (pass-update-buffer))))

    (advice-add #'pass-insert-generated :override #'my/pass-insert-generated)))

(use-package auth-source
  :init
  (progn
    (setq auth-source-debug t)
    (setq auth-source-do-cache t)))

(use-package auth-source-pass
  :demand t
  :after auth-source
  :init
  (progn
    (setq auth-sources '(password-store))))

(use-package avy
  :bind* (("C-," . avy-goto-char-2))
  :bind (("M-g g" . avy-goto-line))
  :init
  (progn
    ;; keys under my fingers (blue keys on my Kinesis Advantage)
    (setq avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o))
    (setq avy-style 'at-full)))

(use-package beginend
  :demand t
  :config
  (progn
    (beginend-global-mode)))

(use-package vdirel
  :disabled t
  :bind (("C-. c" . vdirel-helm-select-email))
  :init
  (progn
    (setq vdirel-repository
          "~/Documents/configuration/contacts/contacts")))

(use-package khardel
  :bind (("C-. c" . khardel-insert-email)))

(use-package nameless
  :hook (emacs-lisp-mode . nameless-mode)
  :init
  (progn
    (setq nameless-affect-indentation-and-filling nil)
    (setq nameless-prefix "…")))

(use-package subword
  :init
  (progn
    (global-subword-mode)))

(use-package company
  :bind ("C-. /" . company-complete)
  :hook (prog-mode . company-mode)
  :init
  (progn
    (setq company-dabbrev-downcase nil)
    (setq company-dabbrev-ignore-case nil)))

(use-package emacs-js
  :hook (js-mode . setup-js-buffer)
  :bind (
         :map js2-refactor-mode-map
         ("<M-up>" . js2r-move-line-up)
         ("<M-down>" . js2r-move-line-down))
  :init
  (progn
    (setq indium-chrome-executable (executable-find "chromium-browser")))
  :config
  (progn
    (setenv "PATH" (concat (getenv "PATH") ":/home/cassou/node_modules/.bin"))
    (add-to-list 'exec-path "/home/cassou/node_modules/.bin")))

(use-package gulp-task-runner
  :commands (gulp))

(use-package prodigy
  :bind (
         :map prodigy-mode-map
         ("k" . (lambda () (interactive) (prodigy-stop t)))))

(use-package finsit-core
  :config
  (progn
    (require 'bookmark)

    (setq finsit-core-monitor-root-location
          (expand-file-name (bookmark-location "ftgp-monitor-root")))))

(use-package libbasecampel
  :init
  (progn
    (setq libbasecampel-client-id (auth-source-pass-get "client_id" "ftgp/37signals.com"))
    (setq libbasecampel-client-secret (auth-source-pass-get "client_secret" "ftgp/37signals.com"))
    (setq libbasecampel-account-id (auth-source-pass-get "account_id" "ftgp/37signals.com"))))

(use-package basecamp
  :config
  (progn
    (let ((expiration-date (format-time-string "%a %b %d %H:%M:%S %Y GMT"
                                               ;; in a week
                                               (time-add nil (* 60 60 24 7)) t)))
      (url-cookie-store "bc3_identity_id"
                        (auth-source-pass-get "bc3_identity_id" "ftgp/37signals.com")
                        expiration-date
                        ".3.basecamp.com" "/" t)

      (url-cookie-store "bc3_session_verification_token"
                        (auth-source-pass-get "bc3_session_verification_token" "ftgp/37signals.com")
                        expiration-date
                        ".3.basecamp.com" "/" t))))

(use-package finsit-basecamp
  :demand t
  :config
  (progn
    (finsit-basecamp-setup)))

(use-package finsit-bugref
  :demand t
  :config
  (progn
    (finsit-bugref-setup)))

(use-package finsit-elcouch
  :demand t
  :after elcouch
  :config
  (progn
    (finsit-elcouch-setup)))

(use-package finsit-magit
  :demand t
  :after magit
  :config
  (progn
    (finsit-magit-setup)))

(use-package finsit-prodigy
  :demand t
  :after prodigy
  :config
  (progn
    (add-to-list 'finsit-prodigy-remotes '("vbox" "http://localhost:8081"))

    (finsit-prodigy-setup)))

(use-package finsit-yasnippet
  :demand t
  :after yasnippet
  :config
  (progn
    (finsit-yasnippet-setup)))

(use-package lui
  :hook (lui-mode . my/lui-setup)
  :init
  (progn
    (setq lui-flyspell-p t)
    (setq lui-flyspell-alist '((".*" "american")))

    (setq lui-time-stamp-position 'right-margin)
    (setq lui-fill-type nil)
    (setq lui-time-stamp-format "%H:%M")

    (setq my/lui-prompt-string
          (format "%s " (propertize "moi>"
                                    'face `(
                                            :foreground "#c678dd"
                                            :background "#48384c"
                                            :weight bold
                                            :box 1)))))
  :config
  (progn
    (defun my/lui-setup ()
      (setq right-margin-width 5)
      (setq fringes-outside-margins t)
      (setq word-wrap t)
      (setq wrap-prefix "    "))))

(use-package lui-logging
  :demand t
  :after lui
  :config
  (progn
    (enable-lui-logging-globally)))

(use-package lui-track-bar
  :demand t
  :after lui
  :config
  (progn
    (enable-lui-track-bar)))

(use-package tracking
  :bind (("C-. ." . tracking-next-buffer))
  :config
  (progn
    (setq tracking-mode-map (make-sparse-keymap))))

(use-package circe
  ;; Install gnutls-utils if circe is stuck "Connecting..."
  ;; https://github.com/jorgenschaefer/circe/issues/287
  :preface
  (progn
    (defun my/circe-get-password (host &optional user)
      "Return password for HOST and USER."
      (when-let* ((entry (auth-source-pass--find-match host user nil)))
        (auth-source-pass-get 'secret entry))))
  :config
  (progn
    (setq circe-default-nick "DamienCassou")
    (setq circe-reduce-lurker-spam t)
    (setq-default circe-sasl-username "DamienCassou")
    (setq-default circe-nickserv-nick "DamienCassou")
    (setq-default circe-sasl-password #'my/circe-get-password)
    (setq-default circe-nickserv-password #'my/circe-get-password)

    (setq circe-prompt-string my/lui-prompt-string)

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
  :after circe
  :hook ((circe-server-connected . enable-circe-notifications))
  :init
  (progn
    (setq circe-notifications-watch-strings '("DamienCassou" "[Dd]amien"))))

(use-package alert
  :demand t
  :init
  (progn
    (setq alert-default-style 'notifications)
    (setq alert-user-configuration
          '(;; throw away eshell notifications if buffer is visible:
            (((:status selected visible)
              (:mode . "\\`eshell-mode\\'"))
             ignore nil)))))

(use-package erc-track
  :init
  (progn
    (setq erc-track-enable-keybindings nil)))

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package yasnippet
  :hook (org-mode . yas-minor-mode)
  :config
  (progn
    (yas-reload-all)))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(use-package editorconfig
  :hook ((prog-mode text-mode) . editorconfig-mode))

(use-package compile
  :hook (compilation-filter . my/colorize-compilation-buffer)
  :config
  (progn
    ;; http://stackoverflow.com/questions/13397737
    (defun my/colorize-compilation-buffer ()
      (require 'ansi-color)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region compilation-filter-start (point))))))

(use-package duplicate-thing
  :bind (("M-D" . duplicate-thing)))

(use-package embrace
  :bind (("C-. ," . embrace-commander))
  :hook (emacs-lisp-mode . embrace-emacs-lisp-mode-hook))

(use-package offlineimap
  :commands offlineimap
  :init
  (progn
    (add-hook 'offlineimap-event-hooks #'my/offlineimap-message-when-done))
  :init
  (progn
    (defun my/offlineimap-message-when-done (message-type &optional action)
      (ignore action)
      (when (string-match "^finished" message-type)
        (let ((count (offlineimap-new-email-count)))
          (alert (cond
                  ((= count 0) "No new email")
                  ((= count 1) "1 new email")
                  ((> count 1) (format "%s new emails" count)))
                 :severity 'low
                 :title "Offlineimap"
                 :buffer (get-buffer "*notmuch-hello*")
                 :icon (if (zerop count)
                           nil
                         (expand-file-name "email.png" (expand-file-name "media" user-emacs-directory)))))))))

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

(use-package make-it-so)

(use-package skeletor
  :commands (skeletor-create-project)
  :init
  (progn
    (setq skeletor-project-directory "/home/cassou/.emacs.d/packages/")
    (setq skeletor-show-project-command 'magit-status)))

(use-package klassified
  :hook (js-mode . klassified-interaction-js-mode))

(use-package json-navigator
  :commands (json-navigator-navigate-region json-navigator-navigate-after-point))

(use-package markdown-mode
  :init
  (progn
    (setq markdown-command "kramdown")))

(use-package devdocs
  :commands (devdocs-search))

(use-package websocket
  :init
  (progn
    (setq websocket-callback-debug-on-error t)
    (setq websocket-debug nil)))

(use-package restclient
  :hook (restclient-response-loaded . my/restclient-use-json-mode)
  :init
  (progn
    (defun my/restclient-use-json-mode ()
      (if (eq major-mode 'js-mode)
          (json-mode)))

    (add-to-list 'auto-mode-alist (cons "\\.restclient\\'" #'restclient-mode))))

(use-package firestarter
  :hook (prog-mode . firestarter-mode)
  :init
  (progn
    (setq firestarter-default-type 'failure)))

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
    (eval-when-compile
      (declare-function eshell/pwd "em-dirs"))

    (defun my/eshell-prompt ()
      (let ((path (abbreviate-file-name (eshell/pwd))))
        (concat
         (propertize
          (format "%s\n$" path)
          'face `(:foreground "#98be65" :background "#3d4a41" :weight bold))
         " "))))
  :init
  (progn
    ;; Inspired from
    ;; https://github.com/Ambrevar/dotfiles/blob/master/.emacs.d/lisp/init-eshell.el
    (setq eshell-history-size 1024)
    (setq eshell-hist-ignoredups t)
    (setq eshell-prompt-function #'my/eshell-prompt)

    ;; If the prompt spans over multiple lines, the regexp should match
    ;; last line only.
    (setq-default eshell-prompt-regexp "^$ ")))

(use-package esh-mode
  :hook (eshell-mode . my/configure-esh-mode)
  :config
  (progn
    (defun my/esh-mode-completion-at-point ()
      "Same as `completion-at-point' except for some commands."
      (interactive)
      ;; unbinding pcomplete/make gives a chance to `bash-completion'
      ;; to complete make rules. Bash-completion is indeed more
      ;; powerfull than `pcomplete-make'.
      (cl-letf (((symbol-function 'pcomplete/make) nil))
        (completion-at-point)))

    ;; We can't use use-package's :bind here as eshell insists on
    ;; recreating a fresh eshell-mode-map for each new eshell buffer.
    (defun my/configure-esh-mode ()
      (bind-key "M-p" #'counsel-esh-history eshell-mode-map)
      (bind-key "<tab>" #'my/esh-mode-completion-at-point eshell-mode-map))))

(use-package em-cmpl
  :hook (eshell-mode . eshell-cmpl-initialize)
  :init
  (progn
    (defun my/eshell-bash-completion ()
      (let ((bash-completion-nospace t))
        (while (pcomplete-here
                (nth 2 (bash-completion-dynamic-complete-nocomint
                        (save-excursion (eshell-bol) (point))
                        (point)))))))

    (when (require 'bash-completion nil t)
      (setq eshell-default-completion-function #'my/eshell-bash-completion))))

(use-package em-smart
  :hook (eshell-mode . eshell-smart-initialize)
  :config
  (progn
    (add-to-list 'eshell-smart-display-navigate-list #'counsel-esh-history)))

(use-package em-term
  :config
  (progn
    (setq eshell-visual-commands
          '("bower" "bluetoothctl" "htop" "jest" "less" "more" "ncdu" "nmtui" "npm" "pinentry-curses" "ssh" "tail" "top" "vim" "watch"))
    (setq eshell-visual-subcommands '(("docker" "build" "pull")
                                      ("yarn" "install" "upgrade-interactive")))

    (add-to-list 'eshell-command-completions-alist '("gunzip" "gz\\'"))
    (add-to-list 'eshell-command-completions-alist '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))

    (setenv "PAGER" "cat") ;; should be unset in .bashrc
    (setenv "SUDO_ASKPASS" (executable-find "pass-root-password.sh"))))

(use-package esh-autosuggest
  :disabled t
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package esh-proc
  :hook ((eshell-kill . my/eshell-command-alert))
  :config
  (progn
    (defun my/eshell-command-alert-cleanup (command)
      "Transforms the list of strings COMMAND into a string for notification."
      ;; remove first element if we don't care about it:
      (if (or (string-suffix-p "bash" (car command))
              (string-suffix-p "/bin/env" (car command))
              (string-suffix-p "/bin/sh" (car command))
              (string-match-p "bin/python[23]?" (car command)))
          ;; call recursively in case we don't care about the second
          ;; one either (e.g., /usr/bin/env bash foo ⇒ foo)
          (my/eshell-command-alert-cleanup (cdr command))
        (let ((binary (file-name-nondirectory (car command)))
              (arguments (cdr command)))
          (mapconcat #'identity (cons binary arguments) " "))))

    ;; https://blog.hoetzel.info/post/eshell-notifications/
    (defun my/eshell-command-alert (process status)
      "Send `alert' with severity based on STATUS when PROCESS finished."
      (let* ((command (my/eshell-command-alert-cleanup (process-command process)))
             (buffer (process-buffer process)))
        (if (string-prefix-p "finished" status)
            (alert command
                   :buffer buffer
                   :severity 'normal
                   :title "Command completed")
          (alert (format "%s\n%s" command (upcase status))
                 :buffer buffer
                 :severity 'urgent
                 :title "Command failed"))))))

(use-package term
  :bind (
         :map term-mode-map
         ("M-j" . my/term-toggle-line-mode)
         :map term-raw-map
         ("M-j" . my/term-toggle-line-mode))
  :init
  (progn
    ;; Avoids checking at runtime if my Emacs is recent enough (it is)
    (setq term--bash-needs-EMACS-status 0)

    (defun my/term-toggle-line-mode ()
      "Toggle between char and line modes."
      (interactive)
      (if (term-in-char-mode)
          (term-line-mode)
        (term-char-mode)))))

(use-package omnisharp
  :after csharp-mode
  :bind (
         :map omnisharp-mode-map
         ("C-c C-r" . omnisharp-run-code-action-refactoring)
         ("M-." . omnisharp-find-implementations)
         ("M-?" . omnisharp-find-usages))
  :hook ((omnisharp-mode . my/configure-omnisharp)
         (csharp-mode . omnisharp-mode))
  :config
  (progn
    (defun my/configure-omnisharp ()
      (add-to-list 'company-backends #'company-omnisharp)
      (local-set-key (kbd "C-c C-c") #'recompile)
      (flycheck-mode 1))))

(use-package webpaste
  :commands (webpaste-paste-buffer webpaste-paste-region))

(use-package vlf ;; handle large/big files
  :demand t)

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (progn
    (pdf-tools-install :no-query)
    (require 'pdf-occur)))

(use-package mpdel
  :demand t
  :init
  (progn
    (setq mpdel-prefix-key (kbd "C-. z")))
  :config
  (progn
    (mpdel-mode)))

(use-package ivy-mpdel
  :after mpdel
  :demand t)

(use-package exwm
  :demand t
  :preface
  (progn
    (defun my/exwm-rename-buffer ()
      "Rename EXWM buffer according to the X class name."
      (when-let* ((new-title
                   (cond
                    ((and
                      (stringp exwm-class-name)
                      (stringp exwm-title)
                      (seq-contains '("Slack") exwm-class-name #'string=))
                     exwm-title)
                    ((stringp exwm-class-name) exwm-class-name))))
        (exwm-workspace-rename-buffer new-title))))

  :hook ((exwm-init . gpastel-mode)
         (exwm-init . display-battery-mode)
         (exwm-init . display-time-mode)
         (exwm-update-class . my/exwm-rename-buffer)
         (exwm-update-title . my/exwm-rename-buffer)))

(use-package exwm-config
  :after exwm
  :demand t)

(use-package exwm-input
  :after exwm
  :demand t
  :preface
  (progn
    (defun my/exwm-counsel-yank-pop ()
      "Same as counsel-yank-pop and paste into exwm buffer."
      (interactive)
      (let ((inhibit-read-only t)
            ;; Make sure we send selected yank-pop candidate to
            ;; clipboard:
            (yank-pop-change-selection t))
        (call-interactively #'counsel-yank-pop))
      (when (derived-mode-p 'exwm-mode)
        ;; https://github.com/ch11ng/exwm/issues/413#issuecomment-386858496
        (exwm-input--set-focus (exwm--buffer->id (window-buffer (selected-window))))
        (exwm-input--fake-key ?\C-v)))

    (defconst my/exwm-applications
      (let ((map (make-hash-table)))
        (require 'map)
        (setf (map-elt map 'firefox)
              (cons "^Firefox$"
                    (lambda () (my/exwm-launch-desktop "firefox.desktop"))))
        (setf (map-elt map 'chromium)
              (cons "^Chromium-browser$"
                    (lambda ()
                      (start-process
                       "chromium-browser"
                       (generate-new-buffer "chromium-process")
                       "chromium-browser"
                       "--remote-debugging-port=9222"))))
        (setf (map-elt map 'pulseaudio)
              (cons "^Pavucontrol$"
                    (lambda () (my/exwm-launch-desktop "pavucontrol.desktop"))))
        (setf (map-elt map 'vbox)
              (cons "^VirtualBox Machine$"
                    (lambda ()
                      (start-process
                       "virtualbox"
                       (generate-new-buffer "virtualbox")
                       "VBoxManage"
                       "startvm"
                       "Windows 10 (v5)"))))
        (setf (map-elt map 'boxes)
              (cons "^Gnome-boxes$"
                    (lambda () (my/exwm-launch-desktop "org.gnome.Boxes.desktop"))))
        (setf (map-elt map 'slack)
              (cons "^Slack | "
                    (lambda () (my/exwm-launch-desktop "com.slack.Slack.desktop"))))
        (setf (map-elt map 'riot)
              (cons "^Riot$"
                    (lambda () (my/exwm-launch-desktop "riot.desktop"))))
        map)
      "Maps an application name symbol to a pair (BUFFER-NAME . DESKTOP-FILENAME).")

    (defun my/exwm-launch-desktop (desktop-filename)
      "Launch the application pointed to by DESKTOP-FILENAME."
      (call-process "gtk-launch" nil 0 nil desktop-filename))

    (defun my/exwm-app-switch/launch (name)
      "Switch to application NAME if already started, start it otherwise.
NAME is a key of `my/exwm-applications'."
      (interactive (list (intern (completing-read "Application" (map-keys my/exwm-applications) nil t))))
      (let* ((application (map-elt my/exwm-applications name))
             (buffer-name (car application))
             (start-application (cdr application)))
        (if-let* ((buffer (seq-find
                           (lambda (buffer)
                             (string-match-p buffer-name
                                             (buffer-name buffer)))
                           (buffer-list))))
            (if (display-buffer-reuse-window buffer '((reusable-frames . visible)))
                (pop-to-buffer buffer
                               (cons
                                #'display-buffer-reuse-window
                                '((reusable-frames . visible))))
              (switch-to-buffer buffer))
          (when (y-or-n-p (format "%s not started yet. Start it?" buffer-name))
            (funcall start-application)))))

    (bind-key  "C-. s f" (lambda () (interactive) (my/exwm-app-switch/launch 'firefox)))
    (bind-key  "C-. s c" (lambda () (interactive) (my/exwm-app-switch/launch 'chromium)))
    (bind-key  "C-. s p" (lambda () (interactive) (my/exwm-app-switch/launch 'pulseaudio)))
    (bind-key  "C-. s v" (lambda () (interactive) (my/exwm-app-switch/launch 'vbox)))
    (bind-key  "C-. s k" (lambda () (interactive) (my/exwm-app-switch/launch 'slack)))
    (bind-key  "C-. s r" (lambda () (interactive) (my/exwm-app-switch/launch 'riot)))

    (defun my/exwm-workspace-switch ()
      "Switch to the other workspace.
This works because I only have 2 workspaces."
      (interactive)
      (let ((workspace-index (if (eq 0 (exwm-workspace--position exwm-workspace--current))
                                 1
                               0)))
        (exwm-workspace-switch-create workspace-index))))

  :config
  (progn
    ;; Key bindings accessible from everywhere:
    (exwm-input-set-key (kbd "s-r") #'exwm-reset)
    (exwm-input-set-key (kbd "s-w") #'my/exwm-workspace-switch)
    (exwm-input-set-key (kbd "s-;") #'other-frame)

    (exwm-input-set-key (kbd "<s-tab>") #'my/swap-last-buffers)
    (exwm-input-set-key (kbd "C-x w") #'my/switch-to-window)
    (exwm-input-set-key (kbd "C-;") #'other-window)
    (exwm-input-set-key (kbd "s-!") #'counsel-linux-app)
    (exwm-input-set-key (kbd "C-M-'") #'shell-switcher-new-shell)
    (exwm-input-set-key (kbd "C-'") #'shell-switcher-switch-buffer)
    (exwm-input-set-key (kbd "C-M-v") #'scroll-other-window)
    (exwm-input-set-key (kbd "M-y") #'my/exwm-counsel-yank-pop)

    ;; Bind C-q so that the next key is sent literally to the
    ;; application
    (add-to-list 'exwm-input-prefix-keys ?\C-q)
    (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

    (add-to-list 'exwm-input-prefix-keys ?\C-.)
    (add-to-list 'exwm-input-prefix-keys ?\C-,)

    (setq exwm-input-simulation-keys
          `(
            ;; movement
            ([?\C-b] . [left])
            ([?\M-b] . [C-left])
            ([?\C-f] . [right])
            ([?\M-f] . [C-right])
            ([?\C-p] . [up])
            ([?\C-n] . [down])
            ([?\C-a] . [home])
            ([S-left] . [C-home])
            ([S-right] . [C-end])
            ([?\C-e] . [end])
            ([?\M-v] . [prior])
            ([?\C-v] . [next])
            ([?\C-d] . [delete])
            ([?\C-k] . [S-end ?\C-x])
            ;; cut/paste, selection
            ([?\C-w] . [?\C-x])
            ([?\M-w] . [?\C-c])
            ([?\C-y] . [?\C-v])
            ([?\M-d] . [C-S-right ?\C-x])
            ([M-backspace] . [C-S-left ?\C-x])
            ;; search
            ([?\C-s] . [?\C-f])
            ;; escape
            ([?\C-g] . [escape])))))

(use-package desktop-environment
  :demand t
  :after exwm-input
  :config
  (progn
    (desktop-environment-mode)))

(use-package buffer-move
  :after exwm-input
  :demand t
  :config
  (progn
    (exwm-input-set-key (kbd "<s-up>") #'buf-move-up)
    (exwm-input-set-key (kbd "<s-down>") #'buf-move-down)
    (exwm-input-set-key (kbd "<s-left>") #'buf-move-left)
    (exwm-input-set-key (kbd "<s-right>") #'buf-move-right)))

(use-package exwm-randr
  :after exwm
  :demand t
  :preface
  (progn
    (defun my/exwm-xrandr ()
      "Configure screen with xrandr."
      (start-process-shell-command
       "xrandr" nil
       "xrandr --output DP-2-1 --primary --right-of eDP-1 --auto"
       ;; Stokholm:
       ;; "xrandr --output DP-1 --primary --above eDP-1 --auto"
       )))

  :hook (exwm-randr-screen-change . my/exwm-xrandr)
  :init
  (progn
    (setq exwm-randr-workspace-monitor-plist '(0 "eDP-1" 1 "DP-2-1"))
    ;; (setq exwm-randr-workspace-monitor-plist '(0 "eDP-1" 1 "DP-1"))
    )
  :config
  (progn
    (exwm-randr-enable)))

(use-package exwm-systemtray
  :after exwm
  :demand t
  :config
  (progn
    (exwm-systemtray-enable)))

(use-package exwm-workspace
  :after exwm
  :demand t
  :init
  (progn
    (setq exwm-workspace-number 2)
    (setq exwm-workspace-show-all-buffers t)
    (setq exwm-layout-show-all-buffers t)))

(use-package exwm-edit
  :after exwm
  :demand t)

(use-package libnetmacs
  :config
  (progn
    (libnetmacs-secretagent-mode)))

(use-package pinentry
  :after exwm
  :demand t
  :init
  (progn
    ;; Use gpg as ssh agent (for this to work in shells, also set the
    ;; ENV var in $HOME/.profile or similar.
    (setenv "SSH_AUTH_SOCK"
            (string-trim
             (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket")))))

(use-package minions
  :demand t
  :config
  (progn
    (minions-mode)))

(use-package youtube-dl
  :bind (
         :map youtube-dl-list-mode-map
         ("k" . youtube-dl-list-kill))
  :init
  (progn
    (setq youtube-dl-directory (expand-file-name "~/Downloads/"))))

(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

(use-package transmission
  :bind (
         :map transmission-mode-map
         ("k" . transmission-remove)))

(use-package nov
  :init
  (progn
    (add-to-list 'auto-mode-alist (cons "\\.epub\\'" #'nov-mode))))

(defmacro my/insert-char-fn (char)
  "Create an anonymous command inserting CHAR."
  `(lambda ()
     (interactive)
     (insert-char ,char)))

;; double arrows
(bind-key "C-x 8 <up>" (my/insert-char-fn ?⇑))
(bind-key "C-x 8 <down>" (my/insert-char-fn ?⇓))
(bind-key "C-x 8 <left>" (my/insert-char-fn ?⇐))
(bind-key "C-x 8 <right>" (my/insert-char-fn ?⇒))

;; simple arrows
(bind-key "C-x 8 <S-up>" (my/insert-char-fn ?↑))
(bind-key "C-x 8 <S-down>" (my/insert-char-fn ?↓))
(bind-key "C-x 8 <S-left>" (my/insert-char-fn ?←))
(bind-key "C-x 8 <S-right>" (my/insert-char-fn ?→))

;; horizontal ellipsis
(bind-key "C-x 8 ," (my/insert-char-fn ?…))

;; Local Variables:
;; eval: (outline-minor-mode)
;; eval: (flycheck-mode -1)
;; no-byte-compile: t
;; End:
