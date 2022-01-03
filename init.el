;;; init.el --- user-init-file                    -*- lexical-binding: t; -*-

(dolist (path load-path)
  (when (string-match-p "/nix/store/[a-z0-9]\\{32\\}-emacs-packages-deps.*" path)
    (dolist (autoload-file (directory-files path t "-autoloads.el"))
      (with-demoted-errors "init.el error: %s"
        (load autoload-file nil t)))))

(setq load-prefer-newer t)

;; Deactivate beeping
(setq ring-bell-function (lambda ()))

;; Apply recommendation from modus Info manual:
(setq face-near-same-color-threshold 45000)

(progn ;    `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require  'borg)
  (borg-initialize))

(progn ; `use-package'
  (setq use-package-always-defer t)
  (setq use-package-enable-imenu-support t)
  (setq use-package-minimum-reported-time 0)
  (setq use-package-verbose t)
  (setq use-package-compute-statistics nil)
  (require 'use-package))

(use-package comp
  :init
  (progn
    (setq native-comp-async-report-warnings-errors nil)))

(use-package auto-compile
  :demand t
  :init
  (progn
    (setq auto-compile-display-buffer nil)
    (setq auto-compile-mode-line-counter t)
    (setq auto-compile-source-recreate-deletes-dest t)
    (setq auto-compile-toggle-deletes-nonlib-dest t)
    (setq auto-compile-update-autoloads t)
    (setq auto-compile-use-mode-line nil))
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
  (setq version-control 'never)
  ;; Delete that when switching to Emacs 28:
  (bind-key "C-x x g" #'revert-buffer))

(progn ; `filelock'
  (setq create-lockfiles nil))

(progn ; `window'
  (bind-key "C-;" #'other-window)
  (unbind-key "C-x o")

  (defun my/swap-last-buffers ()
    "Replace currently visible buffer by last one."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer))))

  (bind-key "<s-tab>" #'my/swap-last-buffers)

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
  :init
  (progn
    ;; Space between windows:
    (setq window-divider-default-right-width 12))
  :config
  (progn
    (defun my/suspend-on-tty-only ()
      "Suspend Emacs, but only if in tty mode."
      (interactive)
      (unless window-system
        (suspend-frame)))

    (defun my/setup-frame (&optional frame)
      "Configure look of FRAME.

If FRAME is nil, configure current frame. If non-nil, make FRAME
current."
      (when frame (select-frame frame))
      (setq frame-title-format "Emacs")
      (when (window-system)
        (setq default-frame-alist
              '((cursor-type bar . 5)
                ;; The width in pixels of the frame’s internal border:
                (internal-border-width . 12)
                ;; large fringes to get hi-resolution flycheck marks:
                (left-fringe    . 8)
                (right-fringe   . 8)
                ;; Hide stuff I don't need:
                (vertical-scroll-bars . nil)
                (tool-bar-lines . 0)
                (menu-bar-lines . 0)))

        (window-divider-mode)

        (set-face-attribute 'default nil :height 100 :family "Fira Mono")
        (set-face-attribute 'fixed-pitch nil :family "Fira Mono" :height 1.0)))

    (my/setup-frame)))

(use-package locate
  :init
  (progn
    (defun my/locate-make-command-line (search-string)
      "Return a list of arguments representing the mlocate command line."
      `("/usr/bin/locate" "--existing" "--regex" "--regexp" ,search-string))

    (setq locate-make-command-line #'my/locate-make-command-line)))

(use-package modus-themes
  :demand t
  :init
  (progn
    (setq modus-themes-mode-line '(moody))
    (setq modus-themes-bold-constructs t)
    (setq modus-themes-completions 'opinionated)
    (setq modus-themes-org-blocks 'greyscale)

    (setq modus-themes-headings
          '((1 . highlight) ; make h1 stand out with a background
            (2 . line)      ; add a line above h2
            (t . rainbow))) ; choose a random color for all headings

    (setq modus-themes-scale-headings t)
    (setq modus-themes-italic-constructs t)

    (setq modus-themes-operandi-color-overrides
          '((fg-window-divider-outer . "white")
            (fg-window-divider-inner . "white")))

    (load-theme 'modus-operandi))
  :config
  (progn
    (modus-themes-load-operandi)

    (with-eval-after-load 'pdf-tools
      ;; Configure PDF page colors. The code below comes from Modus
      ;; Info manual (Backdrop for pdf-tools (DIY)).
      (defun my/pdf-tools-backdrop ()
        "Change background to be different from standard background."
        (face-remap-add-relative
         'default
         `(:background ,(modus-themes-color 'bg-alt))))

      (defun my/pdf-tools-midnight-mode-toggle ()
        "Change background of pdf-view-mode to adapt to current Modus theme."
        (when (eq major-mode 'pdf-view-mode)
          (if (eq (car custom-enabled-themes) 'modus-vivendi)
              (pdf-view-midnight-minor-mode 1)
            (pdf-view-midnight-minor-mode -1))
          (my/pdf-tools-backdrop)))

      (add-hook 'pdf-tools-enabled-hook #'my/pdf-tools-midnight-mode-toggle)
      (add-hook 'modus-themes-after-load-theme-hook #'my/pdf-tools-midnight-mode-toggle))))

(use-package tramp
  :config
  (progn
    (add-to-list 'tramp-remote-path "/run/current-system/profile/bin")))

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
    (setq eval-expression-print-level 10))
  :config
  (progn
    (defun my/join-line ()
      "Join current line and the next."
      (interactive)
      (join-line -1))

    (column-number-mode)))

(use-package so-long
  :demand t
  :config
  (progn
    ;; webpack adds quite a few short lines of initialization in its
    ;; build files. We should let so-long ignore those and still find
    ;; the long ones:
    (setq so-long-max-lines 100)
    (global-so-long-mode)))

(use-package server
  :config
  (progn
    (unless (or (daemonp) (server-running-p))
      (server-start))))

(use-package minibuffer
  :init
  (progn
    (setq read-file-name-completion-ignore-case t)))

(use-package saveplace
  :demand t
  :config
  (progn
    (save-place-mode)))

(use-package tooltip
  :config
  (progn
    ;; Use echo area for help texts instead of a pop-up window:
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
  ;; Activate auto-revert for dired buffers which are not included in
  ;; `global-auto-revert-mode':
  :hook (dired-mode . auto-revert-mode)
  :init
  (progn
    ;; Don't show messages when auto revert happens:
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
    (setq debbugs-gnu-trunk-directory "~/Documents/projects/emacs/emacs-src-26")))

(use-package goto-addr
  :bind (
         :map goto-address-highlight-keymap
         ("C-c C-o" . goto-address-at-point))
  :hook (((nix-mode magit-process-mode) . goto-address-mode)))

(use-package bug-reference
  :bind ((
          :map bug-reference-map
          ("C-c C-o" . bug-reference-push-button))))

(use-package info
  :bind (
         :map Info-mode-map
         ("C-c C-o" . Info-follow-nearest-node)))

(use-package info-colors
  :demand t
  :after info
  :hook (Info-selection . info-colors-fontify-node))

(use-package smime
  :config
  ;; https://src.fedoraproject.org/rpms/emacs/blob/f27/f/default.el
  (setq smime-CA-directory "/etc/ssl/certs"))

(use-package display-line-numbers
  :hook ((prog-mode text-mode) . my/display-line-numbers)
  :config
  (progn
    (defun my/display-line-numbers ()
      (when buffer-file-name
        (display-line-numbers-mode)))))

(use-package hl-line
  :demand t
  :config
  (progn
    (global-hl-line-mode)))

(use-package lin
  :demand t
  :after hl-line
  :config
  (progn
    (lin-add-to-many-modes)))

(use-package undo-tree
  :demand t
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-auto-save-history nil)
    (setq undo-tree-enable-undo-in-region t)
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

(use-package dired-rsync
  :config
  (progn
    (defun my/dired-rsync-update-modeline (&optional err ind)
      (let ((job-count (length (dired-rsync--get-active-buffers))))
        (cond
         ;; error has occurred
         (err (alert (format "%d job failed: %s" job-count err)
                     :severity 'urgent
                     :title "dired-rsync"))
         ((zerop job-count) (alert "done"
                                   :severity 'normal
                                   :title "dired-rsync")))))

    (advice-add #'dired-rsync--update-modeline :after #'my/dired-rsync-update-modeline)))

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
  :init
  (progn
    (setq magit-diff-refine-hunk t)
    (setq magit-process-find-password-functions '(magit-process-password-auth-source))
    (setq magit-branch-prefer-remote-upstream '("master"))
    (setq magit-branch-adjust-remote-upstream-alist '(("origin/master" "master")))
    (setq magit-module-sections-nested nil)
    (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
    (setq magit-no-confirm '(amend-published))
    (setq magit-revision-insert-related-refs nil)
    (setq magit-revision-show-gravatars t)
    (setq magit-clone-set-remote.pushDefault t))
  :config
  (progn
    ;; Enable magit-clean
    (put 'magit-clean 'disabled nil)

    ;; Add modules in magit status buffer:
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-modules
                            'magit-insert-unpulled-from-upstream)

    ;; Only show the module sections I'm interested in
    (with-eval-after-load "magit-submodule"
      (remove-hook 'magit-module-sections-hook 'magit-insert-modules-overview)
      (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpulled-from-pushremote)
      (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-upstream)
      (remove-hook 'magit-module-sections-hook 'magit-insert-modules-unpushed-to-pushremote))

    (transient-replace-suffix 'magit-commit 'magit-commit-autofixup
      '("x" "Absorb changes" magit-commit-absorb))))

(use-package magit-extras
  :demand (project magit))

(use-package magit-tbdiff
  :demand t
  :after magit)

(use-package moody
  :demand t
  :config
  (progn
    (defun my/moody-flycheck-status ()
      "Return the status of flycheck to be displayed in the mode-line."
      (when (and (featurep 'flycheck) flycheck-mode)
        (let ((text (pcase flycheck-last-status-change
                      (`finished (if flycheck-current-errors
                                     (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                                    (+ (or .warning 0) (or .error 0)))))
                                       (format "✖ %s Issue%s" count (if (eq 1 count) "" "s")))
                                   ""))
                      (`running     "⟲ Running")
                      (`no-checker  "⚠ No Checker")
                      (`not-checked "✖ Disabled")
                      (`errored     "⚠ Error")
                      (`interrupted "⛔ Interrupted")
                      (`suspicious  ""))))
          (if (string-empty-p text)
              ""
            (list text " ")))))

    (defvar my/moody-mule-info
      '(:eval (unless (eq buffer-file-coding-system
                          (default-value 'buffer-file-coding-system))
                (list mode-line-mule-info " "))))
    (put 'my/moody-mule-info 'risky-local-variable t)
    (make-variable-buffer-local 'my/moody-mule-info)

    (defvar my/moody-modified
      '(:eval (if (buffer-modified-p (current-buffer)) "x " "")))
    (put 'my/moody-modified 'risky-local-variable t)
    (make-variable-buffer-local 'my/moody-modified)

    (defvar my/moody-flycheck
      '(:eval (my/moody-flycheck-status)))
    (put 'my/moody-flycheck 'risky-local-variable t)
    (make-variable-buffer-local 'my/moody-flycheck)

    (defvar my/moody-mode-line-client "")

    (defvar my/moody-buffer-position (list mode-line-percent-position " %l:%c "))

    (setq x-underline-at-descent-line t)
    (moody-replace-mode-line-buffer-identification)
    (moody-replace-vc-mode)

    (moody-replace-element 'mode-line-mule-info 'my/moody-mule-info)
    (moody-replace-element 'mode-line-position 'my/moody-buffer-position)
    (moody-replace-element 'mode-line-modified 'my/moody-modified)
    (moody-replace-element 'mode-line-remote 'my/moody-flycheck)
    (moody-replace-element 'mode-line-client 'my/mode-line-client)))

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
  :after (:any helpful info)
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

(use-package checkdoc
  :init
  (progn
    (setq checkdoc-spellcheck-documentation-flag t)))

(use-package flymake
  :bind (
         ;; Use the same bindings as flycheck:
         :map flymake-mode-map
         ("C-c ! n" . flymake-goto-next-error)
         ("C-c ! p" . flymake-goto-prev-error)
         ("C-c ! l" . flymake-show-diagnostics-buffer)))

(use-package flycheck
  :init
  (progn
    (setq flycheck-emacs-lisp-load-path 'inherit)))

(use-package flycheck-hledger
  :after (flycheck ledger-mode)
  :demand t
  :init
  (progn
    (setq flycheck-hledger-strict t)
    (setq flycheck-hledger-checks '("payees" "ordereddates"))))

(use-package flycheck-package
  :demand t
  :after flycheck
  :config
  (progn
    (flycheck-package-setup)))

(use-package ledger-mode
  :hook (ledger-mode . my/configure-ledger-mode)
  :mode "\\.hledger\\'"
  :bind (
         :map ledger-mode-map
         ("C-c C-r" . ledger-report)
         ;; To get outline-minor-mode in ledger buffers:
         ("TAB" . org-cycle)
         :map ledger-report-mode-map
         ("C-c C-r" . ledger-report))
  :init
  (progn
    (setq ledger-reports
          (mapcar
           (lambda (pair)
             (list (car pair)
                   (format "%s %s"
                           "%(binary) -f %(ledger-file)"
                           (cdr pair))))
           '(("Account statement" . "register --auto ^%(account)")
             ("Income statement"  . "balance --auto --tree --period %(month) --invert ^income ^expense")
             ("Balance sheet"     . "balance --auto --tree ^asset ^debt \"^equity:\"")
             ("Budget"            . "balance --auto --tree --empty ^budget not:unbudgeted"))))

    ;; For ledger
    (progn
      (setq ledger-mode-should-check-version nil)
      (setq ledger-binary-path (executable-find "ledger"))
      (setq ledger-report-links-in-register t)
      (setq ledger-report-native-highlighting-arguments '("--color" "--force-color"))
      (setq ledger-report-auto-width t))

    ;; For hledger
    (progn
      (setq ledger-mode-should-check-version nil)
      (setq ledger-binary-path (executable-find "hledger"))
      (setq ledger-report-links-in-register nil)
      (setq ledger-report-native-highlighting-arguments '("--color=always"))
      (setq ledger-report-auto-width nil))

    (setq ledger-reconcile-default-commodity "EUR")
    (setq ledger-report-use-header-line t)
    (setq ledger-report-use-native-highlighting t)
    (setq ledger-report-auto-refresh-sticky-cursor t)
    (setq ledger-report-use-strict t)
    (setq ledger-highlight-xact-under-point nil)
    (setq ledger-copy-transaction-insert-blank-line-after t)

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

    (defun my/ledger-configure-outline-minor-mode ()
      "Configure a ledger buffer when `outline-minor-mode' is active."
      (font-lock-add-keywords 'ledger-mode outline-font-lock-keywords)
      (setq-local ;; copied from outline-mode major mode:
       imenu-generic-expression
       (list (list nil (concat "^\\(?:" outline-regexp "\\).*$") 0))))

    (defun my/configure-ledger-mode ()
      "Configure the current Ledger buffer."
      ;; use TAB to complete:
      (setq-local tab-always-indent 'complete)
      (add-hook 'outline-minor-mode-hook #'my/ledger-configure-outline-minor-mode nil t)))
  :config
  (progn
    (let ((date-format "%A, %B %-e"))
      (defun my/ledger-insert-dates ()
        "Insert all dates of a year in the current buffer as headings."
        (let* ((year 2022)
               (day `(0 0 0 1 1 ,year 0 t 0))
               (inc-day (make-decoded-time :day 1)))
          (while (equal (decoded-time-year day) year)
            (when (equal (decoded-time-day day) 1)
              (insert (format "** %s\n" (format-time-string "%B" (encode-time day)))))
            (insert (format "*** %s\n" (format-time-string date-format (encode-time day))))
            ;; day ⇐ day + 1
            (setq day (decoded-time-add day inc-day)))))

      (defun my/ledger-position-at-date (moment)
        "Move point in current buffer to insert new transaction at MOMENT.
MOMENT is an encoded date."
        (let ((heading (format "*** %s" (format-time-string date-format moment))))
          (setf (point) (point-min))
          (search-forward heading)
          (forward-line)
          (re-search-forward "^\\*\\*\\*" nil t)
          (setf (point) (line-beginning-position)))))

    (advice-add #'ledger-xact-find-slot :override #'my/ledger-position-at-date)))

(use-package ledger-complete
  :init
  (progn
    (setq ledger-complete-in-steps nil)))

(use-package ledger-import
  :hook ((ledger-import-finished . my/ledger-import-finish))
  :config
  (progn
    (setq ledger-import-boobank-import-from-date "2021-12-30")
    (setq ledger-import-autosync-command
          '("hledger-autosync" "--assertions"
            "--payee-format" "{payee}"))

    (defun my/ledger-import-alert ()
      "Notify the user that import is finished."
      (alert "Finished"
             :title "Ledger-autosync"
             :buffer (current-buffer)))

    (defun my/ledger-import-remove-EUR ()
      "Remove the EUR commodity in the current buffer."
      (setf (point) (point-min))
      (while (search-forward " EUR" nil t)
        (replace-match ""))
      (ledger-mode-clean-buffer))

    (defun my/ledger-import-merge-autosync-transactions ()
      "Merge all autosync transactions into just one."
      (setf (point) (point-min))
      (search-forward "Autosync Balance Assertion")
      (delete-matching-lines "Autosync Balance Assertion")
      (delete-matching-lines "^$"))

    (defun my/ledger-import-add-today-date-as-outline ()
      "Add today's date as `outline-mode' markup."
      (setf (point) (point-min))
      (search-forward "Autosync Balance Assertion")
      (setf (point) (line-beginning-position))
      (insert (format "*** %s\n\n" (format-time-string "%B %-d"))))

    (defun my/ledger-import-finish ()
      "Some actions to do when ledger-import finishes."
      (interactive)
      (my/ledger-import-remove-EUR)
      (my/ledger-import-merge-autosync-transactions)
      (my/ledger-import-add-today-date-as-outline)
      (my/ledger-import-alert))

    ;; Fill `ledger-import-accounts' and `ledger-import-ofx-rewrite-rules':
    (let ((file (expand-file-name "~/.password-store/Secure_Notes/ledger-accounts.gpg")))
      (when (file-exists-p file)
        (load file t)))))

(use-package goggles
  :demand t
  :hook ((text-mode prog-mode) . goggles-mode)
  :config
  (progn
    (goggles-mode)))

(use-package org
  :bind
  (("C-. o a"   . org-agenda)
   ("C-. o l"   . org-store-link)
   ("C-. o s"   . org-save-all-org-buffers)
   ("C-. o t"   . org-capture))
  :init
  (progn
    (setq org-babel-load-languages '((shell . t) (emacs-lisp . t) (dot . t) (R . t) (python . t)))
    (setq org-catch-invisible-edits 'show-and-error)
    (setq org-cycle-separator-lines 0)
    (setq org-clock-clocked-in-display nil)
    (setq org-id-link-to-org-use-id t)
    (setq org-adapt-indentation nil)
    (setq org-directory "~/configuration/org")
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

    (defun my/string-truncate (len s)
      "If S is longer than LEN, cut it down and add \"…\" to the end.

The resulting string, including ellipsis, will be LEN characters
long."
      (declare (pure t) (side-effect-free t))
      (let ((ellipsis "…"))
        (if (> (length s) len)
            (format "%s%s" (substring s 0 (- len (length ellipsis))) ellipsis)
          s)))

    (defun org-agenda-format-parent (n)
      (save-excursion
        (save-restriction
          (widen)
          (org-up-heading-safe)
          (my/string-truncate n (org-get-heading t t)))))

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
    (unbind-key "<S-right>" org-agenda-mode-map)))

(use-package ox
  :config
  (progn
    (defun my/org-export-new-reference (references)
      "Override of `org-export-new-reference' so identifiers are stable.
This is useful when generating presentations with ox-reveal
because slides don't change their ID all the time."
      (length references))

    (advice-add #'org-export-new-reference
                :override
                #'my/org-export-new-reference)))

(use-package ox-twbs
  :demand t
  :after org)

(use-package ox-reveal
  :init
  (progn
    (setq org-reveal-root
          (concat "file://" (expand-file-name "~/Documents/projects/reveal/latest")))
    (setq org-reveal-title-slide nil)))

(use-package org-caldav
  :bind (("C-. o S"   . org-caldav-sync))
  :config
  (progn
    (setq org-caldav-url "https://dav.petton.fr/damien"
          org-caldav-calendar-id "92d00156-d9fd-7188-977b-af2f48c47c8a"
          org-caldav-inbox org-default-calendar-file
          org-caldav-files '()
          org-icalendar-timezone "Europe/Berlin"
          org-caldav-sync-changes-to-org 'all)

    (defun my/org-caldav-archive-year ()
      "Archive a given year in my calendar."
      (while (re-search-forward "^ *<2020-.*>$" nil t)
        (org-archive-subtree)))))

(use-package org-roam
  :demand t
  :bind (("C-. r r" . org-roam-capture)
         ("C-. r f" . org-roam-node-find)
         :map org-mode-map
         ("C-. r i" . org-roam-node-insert)
         ("C-. r b" . org-roam-buffer-toggle)
         :map org-roam-mode-map
         ("C-. r b" . org-roam-buffer-toggle))
  :init
  (progn
    (setq org-roam-directory (file-truename "/home/cassou/configuration/org-roam"))

    (setq org-roam-mode-section-functions
          (list #'org-roam-backlinks-section
                #'org-roam-reflinks-section
                #'org-roam-unlinked-references-section))

    (setq org-roam-capture-templates
          '(("d" "default" plain "%?"
             :target (file+head "%<%Y-%m-%d>-${slug}.org" "#+title: ${title}\n#+created: %T\n\n")
             :unnarrowed t))))
  :config
  (progn
    (org-roam-db-autosync-mode)))

(use-package org-roam-dailies
  :load-path "lib/org-roam/extensions"
  :demand t
  :after org-roam
  :bind (("C-. r d" . org-roam-dailies-map))
  :init
  (progn
    (setq org-roam-dailies-directory "daily")))

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode))

(use-package calendar
  :init
  (progn
    (setq calendar-date-style 'european)
    (setq calendar-week-start-day 1)
    (setq calendar-mark-holidays-flag t)))

(use-package drag-stuff
  :demand t
  :config
  (progn
    (drag-stuff-global-mode)
    (drag-stuff-define-keys)
    (dolist (mode '(org-mode rebase-mode emacs-lisp-mode mpdel-playlist-current-playlist-mode))
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

(use-package dumb-jump
  :init
  (progn
    (with-eval-after-load "xref"
      (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate))))

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
    (setq notmuch-archive-tags '("-inbox" "-unread"))
    (setq notmuch-hello-sections '(notmuch-hello-insert-saved-searches))
    (setq notmuch-search-oldest-first nil)
    (setq notmuch-draft-save-plaintext t)
    (setq notmuch-fcc-dirs "Perso/Sent")
    (setq notmuch-identities '("Damien Cassou <damien@cassou.me>"))

    (setq notmuch-saved-searches
          `((:name "inbox" :query ,"(folder:\"Perso/INBOX\")" :key "i")
            (:name "sent" :query "from:damien@cassou.me" :key "s")))))

(use-package notmuch-show
  :bind (
         :map notmuch-show-mode-map
         ;; bind 'r' to reply-all, and 'R' to reply
         ("r" . notmuch-show-reply)
         ("R" . notmuch-show-reply-sender)
         :map notmuch-show-part-map
         ("d" . my/notmuch-show-ics-to-org-part))
  :init
  (progn
    (setq notmuch-show-imenu-indent t)
    (setq notmuch-message-headers '("To" "Cc" "Subject" "Date"))

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

(use-package message
  :init
  (progn
    (setq message-log-max t)
    (setq message-send-mail-function 'message-send-mail-with-sendmail)
    (setq message-signature t)
    (setq message-signature-file "~/.signature"))
  :config
  (progn
    ;; Add "Fwd:" to the beginning of Subject of forwarded emails so that
    ;; basecamp detects it properly:
    (unless (listp message-make-forward-subject-function)
      (setq message-make-forward-subject-function (list message-make-forward-subject-function)))

    (add-to-list 'message-make-forward-subject-function #'message-forward-subject-fwd)))

(use-package paredit
  :bind (
         :map paredit-mode-map
         ("M-s" . nil))
  :hook ((emacs-lisp-mode
          lisp-mode
          eval-expression-minibuffer-setup
          scheme-mode
          geiser-repl-mode) . enable-paredit-mode)
  :config
  (progn
    (with-eval-after-load "eldoc"
      (eldoc-add-command #'paredit-backward-delete #'paredit-close-round))))

(use-package elec-pair
  :demand t
  :config
  (progn
    (electric-pair-mode)))

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
  :bind (([remap describe-key] . helpful-key)
         ([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)))

(use-package aggressive-indent
  :hook ((lisp-mode emacs-lisp-mode scheme-mode) . aggressive-indent-mode))

(use-package bookmark
  :init
  (progn
    (setq bookmark-save-flag 1)))

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
  :bind (
         :map pass-mode-map
         ("M-w" . pass-copy))
  :config
  (progn
    (defun my/pass-insert-generated (entry)
      "Same as `pass-insert-generated' but with my own template."
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

(use-package prodigy
  :bind (
         :map prodigy-mode-map
         ("k" . (lambda () (interactive) (prodigy-stop t)))
         ("M-w" . prodigy-copy-url)
         ("y" . finsit-prodigy-start-cypress)))

(use-package finsit-core
  :config
  (progn
    (require 'bookmark)

    (setq finsit-core-monitor-root-location
          (expand-file-name (bookmark-location "ftgp-monitor-root")))))

(use-package libbcel
  :config
  (progn
    (setq libbcel-oauth-store-encryption-keys (list "8E64FBE545A394F5D35CD202F72C652AE7564ECC"))
    (setq libbcel-oauth-client-id (auth-source-pass-get "client_id" "ftgp/37signals.com"))
    (setq libbcel-oauth-client-secret (auth-source-pass-get "client_secret" "ftgp/37signals.com"))
    (setq libbcel-client-account-id (auth-source-pass-get "account_id" "ftgp/37signals.com"))))

(use-package finsit-basecamp
  :demand t
  :after (finsit-magit)
  :config
  (progn
    (finsit-basecamp-setup)))

(use-package finsit-bugref
  :demand t
  :config
  (progn
    (finsit-bugref-setup)))

(use-package libelcouch
  :init
  (progn
    (setq libelcouch-timeout 100)))

(use-package finsit-elcouch
  :demand t
  :after elcouch
  :config
  (progn
    (finsit-elcouch-setup)))

(use-package indium
  :bind (
         :map indium-interaction-mode-map
         ("C-c r" . indium-reload)
         :map indium-repl-mode-map
         ("C-c r" . indium-reload)
         ("C-c d" . indium-switch-to-debugger)))

(use-package finsit-javascript
  :demand t
  :after js
  :config
  (progn
    (finsit-javascript-setup)
    (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "lib/ftgp/snippets"))
    (yas-reload-all)

    (set-face-attribute 'finsit-javascript-html-tag-face nil :background nil)))

(use-package finsit-magit
  :demand t
  :after magit
  :config
  (progn
    (add-to-list 'finsit-magit-projects '("Period report refactoring" . "24152873"))

    (finsit-magit-setup)))

(use-package finsit-prodigy
  :demand t
  :after prodigy
  :config
  (progn
    (add-to-list 'finsit-prodigy-remotes '("boxes" "http://192.168.122.131:80"))
    (finsit-prodigy-setup)))

(use-package alert
  :demand t
  :init
  (progn
    (setq alert-default-style 'notifications)))

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package yasnippet
  :hook (((org-mode git-commit-mode) . yas-minor-mode))
  :config
  (progn
    (yas-reload-all)))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(use-package editorconfig
  :hook ((prog-mode text-mode) . editorconfig-mode)
  :config
  (progn
    ;; https://github.com/editorconfig/editorconfig-emacs/issues/246
    (add-to-list 'editorconfig-exclude-modes 'git-rebase-mode)))

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

(use-package smtpmail
  :init
  (progn
    (setq smtpmail-debug-info t)
    (setq smtpmail-debug-verb t)
    (setq smtpmail-stream-type 'starttls)))

(use-package sendmail
  :init
  (progn
    (setq send-mail-function 'smtpmail-send-it)
    (setq sendmail-program "msmtp")))

(use-package firestarter
  :hook (prog-mode . firestarter-mode)
  :init
  (progn
    (setq firestarter-default-type 'failure)
    (setq firestarter-auto-kill t))
  :config
  (progn
    (defun my/firestarter-alert (process)
      "Alert the user based on PROCESS termination."
      (let ((return-code (process-exit-status process))
            (buffer-name (process-get process 'buffer-name))
            (output (process-get process 'output))
            end)
        (cl-case return-code
          (0 (alert "success"
                    :title "firestarter"))
          (otherwise (alert output
                            :title "firestarter"
                            :severity 'urgent)))))

    (add-to-list 'firestarter-reporting-functions #'my/firestarter-alert)))

(use-package vterm
  :bind (
         :map vterm-mode-map
         ("<f8>" . vterm-send-C-x))
  :init
  (progn
    (setq vterm-kill-buffer-on-exit t)
    (setq vterm-max-scrollback 100000)
    (setq vterm-buffer-name-string "vterm: %s")

    (defun my/vterm-open-new ()
      "Open a new vterm session."
      (interactive)
      (vterm (generate-new-buffer-name "vterm")))

    (with-eval-after-load 'shell-switcher
      (setq shell-switcher-new-shell-function 'my/vterm-open-new))))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (progn
    (pdf-tools-install :no-query)
    (require 'pdf-occur)))

(use-package libmpdel
  :hook ((libmpdel-current-song-changed . my/libmpdel-write-song-to-file)
         (libmpdel-player-changed . my/libmpdel-write-song-to-file))
  :config
  (progn
    (defun my/libmpdel-write-song-to-file ()
      "Write current song name to a dedicated file."
      (with-current-buffer (find-file-noselect "/tmp/i3status-current-song.log")
        (let ((song (libmpdel-current-song)))
          (erase-buffer)
          (when (and song (libmpdel-playing-p))
            (insert (format "♪ %s by %s"
                            (libmpdel-entity-name song)
                            (libmpdel-entity-name (libmpdel-artist song)))))
          (save-buffer))))))

(use-package mpdel
  :demand t
  :bind (
         :map mpdel-core-map
         ("n" . mpdel-core-open-artists)
         ("p" . mpdel-core-insert-current-playlist)
         :map mpdel-playlist-current-playlist-mode-map
         ("p" . mpdel-playlist-play))
  :init
  (progn
    (setq mpdel-prefix-key (kbd "C-. z")))
  :config
  (progn
    (mpdel-mode)))

(use-package ivy-mpdel
  :after mpdel
  :demand t)

(use-package minions
  :demand t
  :config
  (progn
    (minions-mode)))

(use-package ytdl
  :hook (ytdl-download-finished . my/ytdl-alert)
  :init
  (progn
    (setq ytdl-music-folder "~/Downloads/music")
    (defun my/ytdl-alert ()
      (alert "youtube-dl"
             :severity 'normal
             :title "Download completed"))))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package ob-verb
  :demand t
  :after org)

(use-package nix-mode
  :hook ((proced-mode . nix-prettify-mode)))

(use-package nix-sandbox
  :demand t
  :init
  (progn
    (with-eval-after-load "flycheck"
      (setq flycheck-command-wrapper-function
            (lambda (command) (apply #'nix-shell-command (nix-current-sandbox) command)))
      (setq flycheck-executable-find
            (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd))))
    (with-eval-after-load "haskell"
      (setq haskell-process-wrapper-function
            (lambda (args) (apply #'nix-shell-command (nix-current-sandbox) args))))))

;; envrc must come late in the init.el file so add-hook adds it first
;; in `find-file-hook'.
(use-package envrc
  :demand t
  :config
  (progn
    (envrc-global-mode)))

(use-package savehist
  :demand t
  :config
  (progn
    (savehist-mode)))

(use-package vertico
  :demand t
  :init
  (progn
    (setq vertico-cycle t))
  :config
  (progn
    (vertico-mode)))

(use-package vertico-directory
  :load-path "lib/vertico/extensions"
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :demand t
  :bind (
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :config
  (progn
    (marginalia-mode)

    (defun my/marginalia-annotate-buffer (cand)
      "Return a string giving information about CAND, a buffer.

This is the same as `marginalia-annotate-buffer' but only keeps
the buffer's filename."
      (when-let (buffer (get-buffer cand))
        (marginalia--fields
         ((if-let (proc (get-buffer-process buffer))
              (format "(%s %s) %s"
                      proc (process-status proc)
                      (abbreviate-file-name (buffer-local-value 'default-directory buffer)))
            (abbreviate-file-name
             (or (cond
                  ;; see ibuffer-buffer-file-name
                  ((buffer-file-name buffer))
                  ((when-let (dir (and (local-variable-p 'dired-directory buffer)
                                       (buffer-local-value 'dired-directory buffer)))
                     (expand-file-name (if (stringp dir) dir (car dir))
                                       (buffer-local-value 'default-directory buffer))))
                  ((local-variable-p 'list-buffers-directory buffer)
                   (buffer-local-value 'list-buffers-directory buffer)))
                 "")))
          :truncate marginalia-truncate-width
          :face 'marginalia-file-name))))

    ;; Use my own annotator when listing buffers:
    (map-put! marginalia-annotator-registry 'buffer (list #'my/marginalia-annotate-buffer))

    ;; I don't want any information whe listing files:
    (map-delete marginalia-annotator-registry 'file)))

(use-package epkg-marginalia
  :demand t
  :after marginalia
  :init
  (progn
    (cl-pushnew 'epkg-marginalia-annotate-package
                (alist-get 'package marginalia-annotator-registry))))

(use-package orderless
  :demand t
  :init
  (progn
    (setq completion-styles '(orderless))))

(use-package xref
  :init
  (progn
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read)))

(use-package project
  :bind (
         :map project-prefix-map
         ;; Use magit as default command when switching projects:
         ("p" . my/project-switch-project-to-magit)
         ("v" . shell-switcher-open-on-project))
  :init
  (progn
    (defun my/project-switch-project-to-magit ()
      "Ask the user to select a project from known projects and open magit on the selection."
      (interactive)
      (magit-status-setup-buffer (project-prompt-project-dir))))
  :config
  (progn
    ;; Delete commands I don't want to see when switching projects:
    (dolist (undesired-switch-command '(project-vc-dir project-eshell))
      (setq project-switch-commands (cl-delete undesired-switch-command project-switch-commands :key #'car)))

    (add-to-list 'project-switch-commands '(shell-switcher-open-on-project "Shell") t)))

(use-package consult
  :bind (;; Virtual Buffers
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap bookmark-jump] . consult-bookmark)
         ;; Editing
         ([remap yank-pop] . consult-yank-from-kill-ring)
         ;; Navigation
         ([remap goto-line] . consult-goto-line)
         ;; Grep and Find
         ([remap project-find-regexp] . consult-ripgrep))
  :init
  (progn
    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref)
    (setq xref-show-definitions-function #'consult-xref)

    ;; Use consult to have in-buffer completions displayed in the minibuffer:
    (setq completion-in-region-function #'consult-completion-in-region)

    ;; Use `project` with consult:
    (setq consult-project-root-function
          (lambda ()
            (when-let (project (project-current))
              (project-root project)))))
  :config
  (progn
    ;; Configure automatic preview of candidates
    (consult-customize
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-file consult--source-project-file consult--source-bookmark
     :preview-key (kbd "M-."))

    (advice-add #'completing-read-multiple
                :override #'consult-completing-read-multiple)

    ;; Remove some sources when listing buffers:
    (dolist (source '(consult--source-bookmark consult--source-project-buffer consult--source-project-file))
      (setq consult-buffer-sources (cl-delete source consult-buffer-sources)))))

(use-package consult-imenu
  :bind (("M-i" . consult-imenu)))

(use-package consult-flycheck
  :after (consult flycheck)
  :bind (
         :map flycheck-command-map
         ("l" . consult-flycheck)))

(use-package consult-flymake
  :after (consult flymake)
  :bind (
         :map flymake-mode-map
         ("C-c ! l" . counselt-flymake)))

(use-package embark
  :demand t
  :bind (("C-S-a" . embark-act)
         :map embark-file-map
         ("v" . shell-switcher-open-on-directory)
         :map embark-bookmark-map
         ("v" . shell-switcher-open-on-bookmark)))

(use-package embark-consult
  :demand t
  :after (embark consult))

(use-package wgrep
  :init
  (progn
    (setq wgrep-enable-key [remap read-only-mode])))

(defun sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (when (file-writable-p file)
    (user-error "File is user writeable, aborting sudo"))
  (find-file (if (file-remote-p file)
                 (concat "/" (file-remote-p file 'method) ":"
                         (file-remote-p file 'user) "@" (file-remote-p file 'host)
                         "|sudo:root@"
                         (file-remote-p file 'host) ":" (file-remote-p file 'localname))
               (concat "/sudo:root@localhost:" file))))

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
