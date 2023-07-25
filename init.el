;;; init.el --- user-init-file                    -*- lexical-binding: t; -*-

(add-to-list 'load-path (locate-user-emacs-file "misc"))

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
  (setq make-backup-files nil)
  (setq version-control 'never))

(progn ; `filelock'
  (setq create-lockfiles nil))

(progn ; `window'
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
  (setq use-short-answers t))

(progn ; `editfns'
  (put 'narrow-to-region 'disabled nil))

(progn ; `subr'
  ;; recommended by
  ;; (info "(embark) How does Embark call the actions?")
  (setq y-or-n-p-use-read-key t))

(progn ; `buffer'
  (defvar-local my/mode-line-buffer-status
      '(buffer-file-name (:eval (cond (buffer-read-only "RO ")
                                      ((buffer-modified-p) "** ")
                                      (t ""))))
    "Return buffer's status: read-only or modified.

Only display something if the buffer is attached to a file and is
either read only or modified.")

  (put 'my/mode-line-buffer-status 'risky-local-variable t)

  (defvar-local my/mode-line-remote
      '(:eval (if (stringp default-directory)
                  (file-remote-p default-directory)
                ""))
    "Return the host if current buffer is remote, an empty string otherwise.")

  (put 'my/mode-line-remote 'risky-local-variable t)

  ;; Change buffer status to my own function to simplify output:
  (setq-default mode-line-format
                (cl-subst 'my/mode-line-buffer-status 'mode-line-modified mode-line-format))

  ;; Change buffer remote info to my own function to simplify output:
  (setq-default mode-line-format
                (cl-subst 'my/mode-line-remote 'mode-line-remote mode-line-format))

  ;; Remove some information I don't need from the modeline:
  (setq-default
   mode-line-format
   (cl-subst-if ""
                (lambda (item) (or
                                (eql item 'mode-line-mule-info)
                                (eql item 'mode-line-client)
                                (eql item 'mode-line-frame-identification)
                                (and (consp item) (eql (car item) 'vc-mode))))
                mode-line-format)))

(use-package custom
  :demand t
  :config
  (progn
    (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
    (when (file-exists-p custom-file)
      (load custom-file))))

(use-package edebug
  :init
  (setq edebug-print-length 5000))

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
      (modify-all-frames-parameters
       '((cursor-type bar . 5)
         (scroll-bar-width . 1)))

      (window-divider-mode))

    (my/setup-frame)))

(use-package window
  :init
  (progn
    (setq switch-to-buffer-obey-display-actions t)

    (defun my/window-balance-windows (&rest args)
      "Same as `balance-windows' but ignores arguments."
      (balance-windows))

    (seq-doseq (fn (list #'split-window #'delete-window))
      (advice-add fn :after #'my/window-balance-windows))))

(use-package emacs-gc-stats
  :demand t
  :config
  (progn
    (setq emacs-gc-stats-gc-defaults 'emacs-defaults)
    (setq emacs-gc-stats-remind t)
    (emacs-gc-stats-mode +1)))

(use-package ffap
  :config
  (progn
    (defun my/ffap-menu-ask (&rest args)
      "Used to override ffap-menu-ask and not show the *Completions* buffer.
This is recommended by Vertico's README."
      (cl-letf (((symbol-function #'minibuffer-completion-help)
                 #'ignore))
        (apply args)))

    (with-eval-after-load "vertico"
      (advice-add #'ffap-menu-ask :around #'my/ffap-menu-ask))))

(use-package isearch
  :init
  (progn
    (setq isearch-allow-motion t)
    (setq isearch-lazy-count t)))

(use-package dabbrev
  :bind (("M-/" . nil)
         ("C-M-/" . nil))
  :init
  (progn
    (setq dabbrev-case-fold-search t)
    (setq dabbrev-case-replace nil))
  :config
  (progn
    (defun my/dabbrev-completion ()
      "Same as `dabbrev-completion' but searches all buffers."
      (interactive)
      (dabbrev-completion '(16)))))

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
    (setq modus-themes-bold-constructs t)
    (setq modus-themes-org-blocks 'greyscale)
    (setq modus-themes-italic-constructs t)

    (setq modus-themes-headings
          '((1 . (1.6))
            (2 . (background 1.5))
            (3 . (background bold 1.2))
            (4 . (1.1))
            (t . ())))

    (load-theme 'modus-operandi))
  :config
  (progn
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
         ("C-x a" . beginning-of-buffer)
         ("M-<" . nil)
         ("C-x e" . end-of-buffer)
         ("M->" . nil)
         ([remap yank-pop] . yank-from-kill-ring)
         ("C-_" . nil) ;; force me to use C-/ to undo
         :map process-menu-mode-map
         ("k" . process-menu-delete-process))
  :init
  (progn
    (setq delete-active-region nil)
    (setq eval-expression-print-length 20)
    (setq eval-expression-print-level 10)
    (setq next-error-message-highlight 'keep)
    (setq set-mark-command-repeat-pop t)
    (setq line-number-mode nil))
  :config
  (progn
    (defun my/join-line ()
      "Join current line and the next."
      (interactive)
      (join-line -1))

    ;; http://mbork.pl/2022-05-23_Copying_code_snippets
    (defun my/copy-snippet-deindented (begin end)
      "Copy region, untabifying and removing indentation."
      (interactive "r")
      (require 'org-macs)
      (let ((orig-tab-width tab-width)
	    (region (buffer-substring-no-properties begin end)))
        (with-temp-buffer
          (setq tab-width orig-tab-width)
          (insert region)
          (untabify (point-min) (point-max))
          (org-do-remove-indentation)
          (kill-new (buffer-string)))))

    (column-number-mode -1)

    ;; Hide commands in M-x which do not work in the current mode.
    (setq read-extended-command-predicate
          #'command-completion-default-include-p)))

(use-package replace
  :bind (
         :map occur-mode-map
         ("C-x C-q" . occur-edit-mode)))

(use-package so-long
  :demand t
  :config
  (progn
    (global-so-long-mode)))

(use-package server
  :init
  (progn
    (setq server-client-instructions nil))
  :config
  (progn
    (unless (or (daemonp) (server-running-p))
      (server-start))))

(use-package elisp-mode
  :hook (emacs-lisp-mode . my/elisp-mode-reduce-mode-name)
  :config
  (progn
    (defun my/elisp-mode-reduce-mode-name ()
      (setq-local mode-name "Elisp"))))

(use-package minibuffer
  :bind (("M-/" . completion-at-point))
  :init
  (progn
    (setq read-file-name-completion-ignore-case t)
    (setq completions-detailed t)))

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

(use-package buffer-mode
  :bind (("C-S-<up>" . buf-move-up)
         ("C-S-<down>" . buf-move-down)
         ("C-S-<left>" . buf-move-left)
         ("C-S-<right>" . buf-move-right)))

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
  :hook (((nix-mode magit-process-mode) . goto-address-mode))
  :config
  (progn
    ;; Recommended by modus-themes (2.7.0 release notes):
    (setq goto-address-mail-face 'link)
    (setq goto-address-mail-mouse-face 'highlight)))

(use-package bug-reference
  :bind ((
          :map bug-reference-map
          ("C-c C-o" . bug-reference-push-button))))

(use-package info
  :bind (
         :map Info-mode-map
         ("C-c C-o" . Info-follow-nearest-node)))

(use-package man
  :init
  (progn
    (setq Man-notify-method 'aggressive)))

(use-package info-variable-pitch
  :hook ((Info-mode . info-variable-pitch-mode)))

(use-package olivetti
  :hook (((Info-mode help-mode helpful-mode eww-mode) . olivetti-mode))
  :init
  (progn
    (setq olivetti-body-width 84)))

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
    (lin-global-mode)))

(use-package undo-tree
  :disabled t
  :demand t
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-auto-save-history nil)
    (setq undo-tree-enable-undo-in-region t)
    (define-key undo-tree-map (kbd "C-x r") nil)))

(use-package vundo
  :bind ("C-x u" . vundo)
  :hook ((vundo-mode . my/vundo-setup))
  :init
  (progn
    (setq vundo-window-max-height 5))
  :config
  (progn
    (setq vundo-glyph-alist vundo-unicode-symbols)

    (defun my/vundo-setup ()
      "Remove mode-line and header-line."
      (setq mode-line-format nil)
      (setq header-line-format nil))))

(use-package ibuffer
  :bind (
         :map ibuffer-mode-map
         ("k". ibuffer-do-my/ibuffer-do-kill-marked))
  :config
  (progn
    (define-ibuffer-op my/ibuffer-do-kill-marked ()
      "Kill marked buffers as with `kill-this-buffer'."
      (:opstring "killed"
                 :active-opstring "kill"
                 :dangerous t
                 :complex t
                 :modifier-p t)
      (if (kill-buffer buf)
          'kill
        nil))))

(use-package dired
  :bind (
         :map dired-mode-map
         ("C-a" . my/dired-move-beginning-of-line)
         ("k" . dired-do-delete)
         ("D" . nil))
  :hook (dired-mode . dired-hide-details-mode)
  :init
  (progn
    (setq dired-auto-revert-buffer t)
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
  :bind ((
          :map magit-mode-map
          ("M-w" . magit-copy-section-value)))
  :init
  (progn
    (setq magit-diff-refine-hunk t)
    (setq magit-process-find-password-functions '(magit-process-password-auth-source))
    (setq magit-branch-prefer-remote-upstream '("master"))
    (setq magit-branch-adjust-remote-upstream-alist '(("origin/master" "master")
                                                      ("origin/main" "main")))
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

(use-package magit-diff
  :bind (
         :map magit-diff-section-map
         ;; disable binding that I use for begining of buffer
         ("C-x a" . nil)))

(use-package magit-extras
  :demand (project magit))

(use-package magit-tbdiff
  :demand t
  :after magit)

(use-package forge
  :demand t
  :after magit
  :hook (forge-post-submit-callback . my/forge-start-timer-for-draft-pullreq)
  :config
  (progn
    (setq-default forge-buffer-draft-p t)

    (defun my/forge-start-timer-for-draft-pullreq (pullreq &rest _)
      "Start a `tmr' timer if PULLREQ is draft."
      (when (map-elt pullreq 'draft)
        (when-let* ((url (map-elt pullreq 'url))
                    (minutes (cond
                              ((string-match-p "foretagsplatsen/monitor" url) 22)
                              (t 10))))
          (require 'tmr)
          (tmr minutes (format "Check draft %s" url) t))))))

(use-package forge-topic
  :init
  (progn
    (setq forge-topic-list-limit '(60 . -1))))

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
  :disabled t
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
  :disabled t
  :bind (("C-. f b" . flyspell-buffer))
  :hook (text-mode . flyspell-mode)
  :init
  (progn
    (setq flyspell-use-meta-tab nil))
  :config
  (progn
    (unbind-key "C-." flyspell-mode-map)
    (unbind-key "C-;" flyspell-mode-map)))

(use-package jinx
  :hook ((emacs-startup . global-jinx-mode)
         (jinx-mode . my/jinx-add-ispell-localwords))
  :bind ([remap ispell-word] . jinx-correct)
  :init
  (progn
    (setq jinx-languages "en_US fr_FR"))
  :config
  (progn
    (add-to-list 'jinx-include-faces
                 (list 'ledger-mode 'ledger-font-comment-face))

    (add-to-list 'jinx-include-faces
                 (list 'js2-mode 'js2-jsdoc-value))

    (defun my/jinx-ispell-localwords ()
      "Return a string of ispell's local words.

Those are the words following `ispell-words-keyword' (usually
\"LocalWords\") in the current buffer."
      (require 'ispell)
      (save-excursion
        (goto-char (point-min))
        (cl-loop while (search-forward ispell-words-keyword nil t)
                 collect (string-trim (buffer-substring-no-properties (point) (line-end-position))) into result
                 finally return (mapconcat #'identity result " "))))

    (defun my/jinx-add-ispell-localwords ()
      "Add ispell's local words to `jinx-local-words'."
      (let ((ispell-localwords (my/jinx-ispell-localwords)))
        (setq jinx-local-words (concat jinx-local-words ispell-localwords))
        (setq jinx--session-words (append jinx--session-words (split-string ispell-localwords)))))

    (defun my/jinx-save-as-ispell-localword (save key word)
      "Save WORD using ispell's `ispell-words-keyword'.
If SAVE is non-nil save, otherwise format candidate given action KEY."
      (if save
          (progn
            (ispell-add-per-file-word-list word)
            (add-to-list 'jinx--session-words word)
            (setq jinx-local-words
                  (string-join
                   (sort (delete-dups
                          (cons word (split-string jinx-local-words)))
                         #'string<)
                   " ")))
        (jinx--save-action key word "File")))

    (map-put! jinx--save-keys ?* #'my/jinx-save-as-ispell-localword)))

(use-package checkdoc
  :init
  (progn
    (setq checkdoc-spellcheck-documentation-flag t)))

(use-package eldoc
  :init
  (progn
    ;; https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
    (add-to-list 'display-buffer-alist
                 '("^\\*eldoc for" display-buffer-at-bottom
                   (window-height . 4)))
    (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))
  :config
  (progn
    ;; https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
    (eldoc-add-command-completions "paredit-")
    (eldoc-add-command-completions "combobulate-")))

(use-package flymake
  :bind (
         ;; Use the same bindings as flycheck:
         :map flymake-mode-map
         ("C-c ! n" . flymake-goto-next-error)
         ("C-c ! p" . flymake-goto-prev-error)
         ("C-c ! l" . flymake-show-buffer-diagnostics)
         ("C-c ! v" . flymake-switch-to-log-buffer))
  :init
  (progn
    (defun my/flymake-modeline ()
      "Compute a modeline format containing flymake's current status."
      (let* ((running-backends (flymake-running-backends))
             (reporting-backends (flymake-reporting-backends))
             (computing-backends (cl-set-difference running-backends reporting-backends)))
        (cond
         ((zerop (hash-table-count flymake--state)) "(?)")
         (computing-backends "(Wait)")
         ((and (flymake-disabled-backends) (null running-backends)) "(Disabled)")
         (t (if (flymake-diagnostics)
                (list "("
                      flymake-mode-line-error-counter
                      flymake-mode-line-warning-counter
                      flymake-mode-line-note-counter
                      ")")
              "")))))

    (setq flymake-suppress-zero-counters t)
    (setq flymake-mode-line-format '(:eval (my/flymake-modeline)))))

(use-package flymake-proc
  :config
  (progn
    ;; flymake-proc adds this legacy backend automatically but (1) I
    ;; don't seem to use it and (2) it triggers warnings in *Flymake
    ;; log*.
    (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)))

(use-package flycheck
  :init
  (progn
    (setq flycheck-emacs-lisp-load-path 'inherit)))

(use-package flycheck-hledger
  :disabled t
  :after (flycheck ledger-mode)
  :demand t
  :init
  (progn
    (setq flycheck-hledger-strict t)
    (setq flycheck-hledger-checks '("payees" "ordereddates" "recentassertions"))))

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
         ("C-c C-c" . my/ledger-lint)
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
      (defun my/ledger-position-at-date (moment)
        "Move point in current buffer to insert new transaction at MOMENT.
MOMENT is an encoded date."
        (let ((heading (format "*** %s" (format-time-string date-format moment))))
          (goto-char (point-min))
          (search-forward heading)
          (forward-line)
          (re-search-forward "; \\*\\*\\*" nil t)
          (goto-char (line-beginning-position)))))

    (advice-add #'ledger-xact-find-slot :override #'my/ledger-position-at-date)

    (defun my/ledger-lint ()
      "Lint my ledger file."
      (interactive)
      (require 'autoclose-shell)
      (autoclose-shell-start "lint-system" '("lint-system")))

    (defun my/ledger-mortgage-read-numbers ()
      "Returns the numbers of the current mortgage reimbursement transaction.

The returned value is of the form (:capital CAPITAL :insurance INSURANCE :interest INTEREST)."
      (cl-labels ((parse-number (string) (string-to-number (string-replace "," "." string))))
        (let* ((number-regexp (rx (1+ (any digit)) ?, (1+ (any digit))))
               (regexp (rx "ECHEANCE PRET"
                           (? " -")
                           " DONT CAP "
                           (group-n 1 (regexp number-regexp))
                           " ASS. "
                           (group-n 2 (regexp number-regexp))
                           "E"
                           (? " -")
                           " INT. "
                           (group-n 3 (regexp number-regexp))
                           (? " COM. 0,00E"))))
          (save-match-data
            (save-excursion
              (ledger-navigate-beginning-of-xact)
              (when-let* (((re-search-forward regexp (line-end-position)))
                          (capital (parse-number (match-string 1)))
                          (insurance (parse-number (match-string 2)))
                          (interest (parse-number (match-string 3))))
                (list :capital capital :insurance insurance :interest interest)))))))

    (defun my/ledger-mortgage-guess-type (numbers)
      "Return the type of the transaction with NUMBERS.
The type is either 'ecoptz, 'immo1 or 'immo2.

NUMBERS is of the form (:capital CAPITAL :insurance INSURANCE :interest INTEREST)."
      (cond
       ((and (>= (map-elt numbers :insurance) 0.1)
             (= (map-elt numbers :interest) 0))
        'ecoptz)
       ((or (>= (map-elt numbers :insurance) 0.1)
            (= (map-elt numbers :interest) 0))
        (user-error "Invalid numbers: %S" numbers))
       ((>= (map-elt numbers :capital) 700) 'immo1)
       (t 'immo2)))

    (defun my/ledger-mortgage-rewrite ()
      "Rewrite the mortgage transaction at point."
      (interactive)
      (when-let* ((numbers (my/ledger-mortgage-read-numbers))
                  (mortgage-type (my/ledger-mortgage-guess-type numbers))
                  (account (format "debt:longterm:mortgage:%s" mortgage-type)))
        (save-match-data
          (save-excursion
            (ledger-navigate-beginning-of-xact)
            (when (re-search-forward " .*$" (line-end-position)) ; skip date
              (replace-match " banque populaire prêt" t)
              (ledger-navigate-end-of-xact)
              (delete-region (line-beginning-position) (line-end-position))
              (map-do
               (lambda (number-type number)
                 (when (> number 0)
                   (insert " " account (symbol-name number-type) "  " (number-to-string number) "\n")))
               numbers)
              (delete-backward-char 1) ; remove additional newline
              (ledger-post-align-dwim))))))))

(use-package flymake-hledger
  :config
  (progn
    ;; Enable 4 optional checks. See URL
    ;; https://hledger.org/1.30/hledger.html#check for the meaning of
    ;; each check and a list of all of them.
    (dolist (check '("ordereddates" "payees" "recentassertions" "tags"))
      (add-to-list 'flymake-hledger-checks check))))

(use-package ledger-complete
  :init
  (progn
    (setq ledger-complete-in-steps nil)))

(use-package ledger-import
  :hook ((ledger-import-finished . my/ledger-import-finish))
  :config
  (progn
    (setq ledger-import-boobank-import-from-date "2023-03-15")
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
      (goto-char (point-min))
      (while (search-forward " EUR" nil t)
        (replace-match ""))
      (ledger-mode-clean-buffer))

    (defun my/ledger-import-merge-autosync-transactions ()
      "Merge all autosync transactions into just one."
      (goto-char (point-min))
      (search-forward "Autosync Balance Assertion")
      (delete-matching-lines "Autosync Balance Assertion")
      (delete-matching-lines "^$"))

    (defun my/ledger-import-add-today-date-as-outline ()
      "Add today's date as `outline-mode' markup."
      (goto-char (point-min))
      (search-forward "Autosync Balance Assertion")
      (goto-char (line-beginning-position))
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

(use-package outli
  :hook (emacs-lisp-mode . outli-mode))

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
    (setq org-insert-heading-respect-content t)
    (setq org-clock-clocked-in-display nil)
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
            ("p" "Appt" entry (file org-default-calendar-file) "* %?\n%^T")
            ("T" "Tickler" entry (file+headline org-default-tickler-file "Tickler") "* %i%? \nSCHEDULED: %^t")))

    (setq org-todo-keywords
          '((sequence "TODO(t)" "|" "DONE(d)" "CANCELLED(c)")
            (sequence "WAITING(w)" "|" "DONE(d)")))

    (setq org-agenda-custom-commands
          '(("a" "Agenda for the current week" ((agenda "" nil)) nil nil)
            ("w" . "TODOs")
            ("d" "30 days deadlines" agenda ""
             ((org-agenda-entry-types '(:deadline))
              (org-agenda-overriding-header "Month deadlines")
              (org-agenda-span 'month)
              (org-agenda-overriding-header "")))))

    (setq org-agenda-show-future-repeats nil)
    (setq org-enforce-todo-dependencies t)
    (setq org-enforce-todo-checkbox-dependencies t)
    (setq org-ellipsis "…")
    (setq org-export-allow-bind-keywords nil)
    (setq org-export-creator-string "")
    (setq org-export-with-toc nil)
    (setq org-fontify-done-headline t)
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
    (add-to-list 'org-modules 'org-protocol)
    (add-to-list 'org-modules 'ox-linuxmag-fr)
    (add-to-list 'org-modules 'org-capture)

    (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

    ;; I use that to switch term buffers
    (unbind-key "C-'" org-mode-map)

    ;; Those are my `beginning-of-buffer' and `end-of-buffer':
    (unbind-key "<S-left>" org-mode-map)
    (unbind-key "<S-right>" org-mode-map)

    (add-to-list 'org-file-apps '("\\.png\\'" . default))))

(use-package org-src
  :init
  (progn
    (setq org-src-ask-before-returning-to-edit-buffer nil)))

(use-package org-id
  :demand t
  :after org
  :init
  (progn
    (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)))

(use-package org-agenda
  :bind (
         :map org-agenda-mode-map
         ("k"         . org-agenda-kill))
  :init
  (progn
    (setq org-agenda-block-separator ?─)
    (setq org-agenda-time-grid
          '((daily today require-timed)
            (800 1000 1200 1400 1600 1800 2000)
            " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
    (setq  org-agenda-current-time-string
           "⭠ now ─────────────────────────────────────────────────"))
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
      (interactive)
      (while (re-search-forward "^ *<2021-.*>$" nil t)
        (org-archive-subtree)))))

(use-package denote
  :bind (("C-. r r" . my/denote-date)
         ("C-. r R" . denote)
         ("C-. r f" . my/denote-find-file))
  :hook (dired-mode . denote-dired-mode)
  :init
  (progn
    (setq denote-date-prompt-use-org-read-date t)
    (setq denote-directory (expand-file-name "~/configuration/denote"))
    (setq denote-known-keywords '("emacs" "beniguet" "école" "Sarah"))
    (setq denote-front-matter-date-format 'org-timestamp)
    (setq denote-dired-directories
          (list denote-directory
                (expand-file-name "attachments" denote-directory))))
  :config
  (progn
    (defun my/denote-find-file (filename)
      "Open FILENAME, a denote file.
Interactively ask which file to open with completion."
      (interactive (list (denote-file-prompt)))
      (find-file filename))

    (defun my/denote-date (&optional date)
      "Open note for DATE and create it if necessary.

DATE is today by default. Interactively, DATE is asked to the
user if the command is called with a prefix argument."
      (interactive (list (and current-prefix-arg
                              (encode-time
                               (iso8601-parse (concat (org-read-date) "T00:00:00"))))))
      (let* ((title (format-time-string "%A %e %B %Y" date))
             (sluggified-title (denote-sluggify title))
             (all-files (denote-all-files))
             (matching-files (seq-filter
                              (apply-partially
                               #'string-match-p
                               (regexp-quote sluggified-title))
                              all-files)))
        (cond
         ((length= matching-files 0) (denote title '("journal")))
         ((length= matching-files 1) (find-file (car matching-files)) (goto-char (point-max)))
         (t (user-error "Several notes in '%s' match '%s'" (denote-directory) sluggified-title)))))))

(use-package calendar
  :init
  (progn
    (setq calendar-date-style 'european)
    (setq calendar-week-start-day 1)
    (setq calendar-mark-holidays-flag t)))

(use-package graphviz-dot-mode
  :init
  (progn
    (setq graphviz-dot-indent-width 2)))

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
  :demand t
  :config
  (progn
    (with-eval-after-load "mm-decode"
      (unify-opening-setup-for-mm-decode))

    (with-eval-after-load "org"
      (unify-opening-setup-for-org))

    (with-eval-after-load "dired-x"
      (unify-opening-setup-for-dired-x))

    (with-eval-after-load "consult"
      (unify-opening-setup-for-consult))))

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

(use-package notmuch-mua
  :demand t
  :config
  (progn
    ;; Configure notmuch as my default mail-user-agent
    (setq mail-user-agent 'notmuch-user-agent)
    (setq compose-mail-user-agent-warnings nil)))

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
    (setq message-signature-file "~/.signature")
    (setq message-mail-user-agent t))
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
         ("M-s" . nil) ; used for isearch
         ("RET" . nil) ; used during M-: to evaluate the input
         ("C-j" . paredit-newline)) ; replacement for RET
  :hook ((emacs-lisp-mode
          lisp-mode
          eval-expression-minibuffer-setup
          scheme-mode
          geiser-repl-mode
          lisp-data-mode) . enable-paredit-mode)
  :config
  (progn
    (with-eval-after-load "eldoc"
      (eldoc-add-command #'paredit-backward-delete #'paredit-close-round))))

(use-package combobulate
  :disabled t
  :hook (js-ts-mode . combobulate-mode))

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
         ("C-l" . find-library))
  :init
  (progn
    (setq describe-bindings-outline t)))

(use-package helpful
  :bind (([remap describe-key] . helpful-key)
         ([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)))

(use-package aggressive-indent
  :hook ((lisp-mode emacs-lisp-mode scheme-mode lisp-data-mode) . aggressive-indent-mode))

(use-package bookmark
  :init
  (progn
    (setq bookmark-save-flag 1)))

(use-package password-store
  :init
  (progn
    (defun my/password-length ()
      "Return a random number suitable for a password length."
      (+ 30 (random 10)))

    (defun my/generate-password (&optional length)
      "Generate a random password of size LENGTH, `my/password-length' by default.

If LENGTH is positive, the password is copied to the kill ring.  If
negative, the password is inserted at point."
      (interactive "P")
      (let* ((length (cond
                      ((eq length '-) (- (my/password-length)))
                      (length length)
                      (t (my/password-length))))
             (password (string-trim (shell-command-to-string
                                     (format "pwgen --num-passwords=1 --secure --symbols %s" (abs length))))))
        (if (< length 0)
            (insert password)
          (kill-new password)
          (message "Added %S to kill ring." password)))))
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
  :bind* (("C-," . avy-goto-char-timer)
          :map isearch-mode-map
          ("C-," . avy-isearch))
  :bind (("M-g g" . avy-goto-line))
  :init
  (progn
    ;; home row on a Colemak keyboard:
    (setq avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?i ?o))
    ;; Let me easily use actions beyond jump:
    (setq avy-single-candidate-jump nil)
    (setq avy-flyspell-correct-function #'ispell-word)
    (setq avy-timeout-seconds 0.4))
  :config
  (progn
    (defmacro my/avy-without-moving-point (point &rest body)
      "Move point to POINT than execute BODY and restore previous location."
      (declare (indent 1))
      `(unwind-protect
           (save-excursion
             (goto-char point)
             ,@body)
         (select-window (cdr (ring-ref avy-ring 0)))))

    (defun my/avy-action-help (point)
      "Show the Emacs help for thing at POINT."
      (my/avy-without-moving-point point
        (helpful-at-point)))

    (defun my/avy-action-kill-whole-line (point)
      "Kill the whole line at POINT."
      (my/avy-without-moving-point point
        (kill-whole-line)))

    (defun my/avy-action-embark (point)
      "Start `embark-act' at POINT."
      (my/avy-without-moving-point point
        (embark-act)))

    (setq avy-dispatch-alist
          '(
            ;; (?a) in avy-keys
            ;; (?b)
            ;; (?c)
            ;; (?d) in avy-keys
            ;; (?e) in avy-keys
            ;; (?f)
            ;; (?g)
            ;; (?h) in avy-keys
            ;; (?i) in avy-keys
            ;; (?j)
            ;; (?k)
            ;; (?l)
            ;; (?m)
            ;; (?n) in avy-keys
            ;; (?o) in avy-keys
            ;; (?p)
            ;; (?q)
            ;; (?r) in avy-keys
            ;; (?s) in avy-keys
            ;; (?t) in avy-keys
            ;; (?u)
            ;; (?v)
            ;; (?w)
            ;; (?x)
            ;; (?y)
            ;; (?z)
            ;; (?A)
            ;; (?B)
            ;; (?C)
            ;; (?D)
            ;; (?E)
            ;; (?F)
            ;; (?G)
            ;; (?H)
            ;; (?I)
            ;; (?J)
            ;; (?K)
            ;; (?L)
            ;; (?M)
            ;; (?N)
            ;; (?O)
            ;; (?P)
            ;; (?Q)
            ;; (?R)
            ;; (?S)
            ;; (?T)
            ;; (?U)
            ;; (?V)
            ;; (?W)
            ;; (?X)
            ;; (?Y)
            ;; (?Z)
            (?$ . avy-action-ispell)
            (?? . my/avy-action-help)
            (?\C-w . avy-action-kill-stay)
            (?\C-k . my/avy-action-kill-whole-line)
            (?\M-w . avy-action-copy)
            (?\C-\S-a . my/avy-action-embark)))))

(use-package beginend
  :demand t
  :config
  (progn
    (beginend-global-mode)))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode))

(use-package repeat
  :demand t
  :config
  (progn
    (repeat-mode)))

(use-package repeat-help
  :disabled t
  :demand t
  :after repeat
  :init
  (progn
    (setq repeat-help-auto nil))
  :config
  (progn
    (repeat-help-mode)))

(use-package khardel
  :bind (("C-. c" . khardel-insert-email)))

(use-package nameless
  :hook (emacs-lisp-mode . nameless-mode)
  :init
  (progn
    (setq nameless-affect-indentation-and-filling nil)
    (setq nameless-prefix "…")))

(use-package epithet
  :hook (((Info-selection eww-after-render help-mode occur shell-mode)
          .
          epithet-rename-buffer)
         (compilation-start . epithet-rename-buffer-ignoring-arguments))
  :config
  (progn
    (add-hook 'compilation-finish-functions #'epithet-rename-buffer-ignoring-arguments)))

(use-package subword
  :init
  (progn
    (global-subword-mode)))

(use-package prodigy
  :bind (("C-. p" . prodigy)
         :map prodigy-mode-map
         ("k" . (lambda () (interactive) (prodigy-stop t)))
         ("M-w" . prodigy-copy-url)
         ("C-<down>" . prodigy-next-with-status)
         ("C-<up>" . prodigy-prev-with-status))
  :init
  (progn
    (setq prodigy-completion-system 'default)))

(use-package treesit-auto
  :disabled t
  :demand t
  :hook (emacs-startup . global-treesit-auto-mode))

(use-package finsit-core
  :config
  (progn
    (require 'bookmark)

    (setq finsit-core-monitor-root-location
          (expand-file-name (bookmark-location "ftgp-monitor-root")))

    (with-eval-after-load 'project
      (defun my/finsit-project-find-file ()
        "Faster alternative to project-find-file for monitor's Client/ folder."
        (interactive)
        (if-let* ((project (project-current))
                  ((eq (car project) 'transient))
                  ((string= (expand-file-name "./" (cdr project))
                            (expand-file-name "./" (finsit-core-monitor-client-location)))))
            (project-find-file-in (thing-at-point 'filename)
                                  (list (finsit-core-monitor-client-location))
                                  (project-current nil (finsit-core-monitor-root-location)))
          (project-find-file)))

      (bind-key "f" #'my/finsit-project-find-file project-prefix-map))))

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

(use-package finsit-javascript
  :demand t
  :after js
  :config
  (progn
    (finsit-javascript-setup)
    (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "lib/ftgp/snippets"))
    (yas-reload-all)

    ;; Remove background: (see https://debbugs.gnu.org/cgi/bugreport.cgi?bug=54156 for details
    (dolist (frame '(nil t))
      (set-face-attribute 'finsit-js-htmlcanvas-html-tag-face frame :background 'unspecified))))

(use-package finsit-js-company
  :config
  (defun my/finsit-js-company-setup ()
    "Prevent configuring `company-mode'.
This should be used as an override of `finsit-js-company-setup'.")

  (advice-add #'finsit-js-company-setup
              :override #'my/finsit-js-company-setup))

(use-package finsit-js-tern
  :config
  (defun my/finsit-js-tern-setup ()
    "Prevent configuring `tern-mode'.
This should be used as an override of `finsit-js-tern-setup'.")

  (advice-add #'finsit-js-tern-setup
              :override #'my/finsit-js-tern-setup))

(use-package finsit-js-flycheck
  :disabled t
  :config
  (defun my/finsit-js-flycheck-setup ()
    "Prevent configuring `flycheck-mode'.
This should be used as an override of `finsit-js-flycheck-setup'.")

  (advice-add #'finsit-js-flycheck-setup
              :override #'my/finsit-js-flycheck-setup))

(use-package related-files
  :bind (("C-x j" . related-files-jump)
         ("C-x J" . related-files-make)))

(use-package related-files-recipe
  :demand t
  :after related-files)

(use-package flymake-eslint
  :hook ((js-mode . my/flymake-eslint-finsit))
  :init
  (progn
    (setq flymake-eslint-executable-args '("--report-unused-disable-directives"))

    (defun my/flymake-eslint-finsit ()
      "Enable flymake-eslint in finsit's Client/ buffer."
      (setq-local flymake-eslint-executable-name (executable-find eslintd-fix-executable))
      (when (finsit-core-own-javascript-buffer-p)
        (setq-local flymake-eslint-project-root (finsit-core-monitor-client-location)))
      (flymake-eslint-enable))))

(use-package eslint-disable-rule
  :after (:all js2-mode (:any flymake flycheck))
  :bind ((
          :map flymake-mode-map
          ("C-c ! k" . eslint-disable-rule-disable-next-line)
          :map flycheck-mode-map
          ("C-c ! k" . eslint-disable-rule-disable-next-line)))
  :init
  (progn
    (setq eslint-disable-rule-require-description 'prefer-description)
    (setq eslint-disable-rule-all-executable "eslint_d")))

(use-package finsit-magit
  :demand t
  :after magit
  :config
  (progn
    (finsit-magit-setup)))

(use-package finsit-forge
  :demand t
  :after forge
  :config
  (progn
    (finsit-forge-setup)))

(use-package finsit-prodigy
  :demand t
  :after prodigy
  :config
  (progn
    (add-to-list 'finsit-prodigy-remotes '("boxes" "http://192.168.122.131:80"))
    (finsit-prodigy-setup)))

(use-package jsonian
  :mode ("\\.json\\'". jsonian-mode)
  :config
  (progn
    ;; json-mode.el automatically modifies auto-mode-alist which I
    ;; don't want:
    (with-eval-after-load "json"
      (setq auto-mode-alist
            (cl-delete 'json-mode auto-mode-alist :key #'cdr)))))

(use-package alert
  :demand t
  :init
  (progn
    (setq alert-default-style 'notifications)))

(use-package diff-hl
  :disabled t
  :demand t
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (after-init . global-diff-hl-mode))
  :init
  (progn
    (setq diff-hl-disable-on-remote t)))

(use-package diff-hl-dired
  :demand t
  :after diff-hl
  :hook ((dired-mode . diff-hl-dired-mode-unless-remote)))

(use-package yasnippet
  :hook (((org-mode git-commit-mode css-mode) . yas-minor-mode))
  :config
  (progn
    (yas-reload-all)))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(use-package editorconfig
  :hook ((prog-mode text-mode) . editorconfig-mode))

(use-package compile
  :hook (compilation-filter . ansi-color-compilation-filter))

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
  :disabled t
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
  :commands (vterm)
  :bind (
         ("C-M-'" . my/vterm-open-new)
         :map vterm-mode-map
         ("<f8>" . vterm-send-C-x)
         ("C-<up>" . vterm-previous-prompt)
         ("C-<down>" . vterm-next-prompt))
  :init
  (progn
    (setq vterm-kill-buffer-on-exit t)
    (setq vterm-max-scrollback 100000)
    (setq vterm-buffer-name-string "vterm: %s")

    (defun my/vterm-open-new ()
      "Open a new vterm session."
      (interactive)
      (vterm (generate-new-buffer-name "vterm")))

    (defun my/vterm-open-on-directory (directory)
      "Open DIRECTORY in vterm."
      (interactive "FDirectory: ")
      (let ((default-directory (if (file-directory-p directory)
                                   directory
                                 (file-name-directory directory))))
        (my/vterm-open-new)))

    (defun my/vterm-open-on-bookmark (bookmark)
      "Open BOOKMARK's location in vterm."
      (interactive (list (bookmark-completing-read "Bookmark: "
				                   bookmark-current-bookmark)))
      (let* ((file (bookmark-location bookmark))
             (default-directory (if (file-directory-p file)
                                    file
                                  (file-name-directory file))))
        (my/vterm-open-new)))

    (defun my/vterm-open-on-project (&optional project)
      "Open the project's root directory in vterm.
If PROJECT is nil, use `project-current'."
      (interactive)
      (when-let* ((project (or project (project-current t)))
                  (default-directory (car (project-roots project))))
        (my/vterm-open-new)))))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (progn
    (pdf-tools-install :no-query)
    (require 'pdf-occur)))

(use-package libmpdel
  :hook ((libmpdel-current-song-changed . my/libmpdel-write-song-to-file)
         (libmpdel-player-changed . my/libmpdel-write-song-to-file))
  :init
  (progn
    (setq libmpdel-music-directory "~/Music/son"))
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

(use-package mpdel-embark
  :demand t
  :after (embark mpdel)
  :config
  (progn
    (mpdel-embark-setup)))

(use-package minions
  :demand t
  :init
  (progn
    (setq minions-mode-line-delimiters nil)

    ;; Always show flymake-mode as this is where flymake report
    ;; problems:
    (setq minions-prominent-modes '(flymake-mode)))
  :config
  (progn
    (minions-mode)))

(use-package moody
  :disabled t
  :demand t
  :config
  (progn
    (setq x-underline-at-descent-line t)

    ;; Add mode-line-position to mode-line-buffer-identification
    (setq-default
     moody-mode-line-buffer-identification
     '(:eval (moody-tab (concat
                         (car (propertized-buffer-identification (buffer-name)))
                         " "
                         (string-replace "%" "%%" (format-mode-line mode-line-position)))
                        20 'down)))

    ;; Remove mode-line-position from mode-line-format
    (setq-default mode-line-format (cl-subst "" 'mode-line-position mode-line-format))

    (moody-replace-mode-line-buffer-identification)
    (moody-replace-mode-line-front-space)))

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
  :disabled t
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
  :disabled t
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
  :config
  (progn
    (vertico-mode)))

(use-package vertico-directory
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-quick
  :demand t
  :after vertico
  :bind (
         :map vertico-map
         ("M-q" . vertico-quick-insert)
         ("C-q" . vertico-quick-exit))
  :init
  (progn
    (setq vertico-quick1 "arstdhn")
    (setq vertico-quick2 "oie")))

(use-package vertico-multiform
  :demand t
  :after vertico
  :config
  (progn
    (vertico-multiform-mode)

    (setq vertico-multiform-commands
          '(;; show grep results in a dedicated buffer:
            (consult-ripgrep buffer)))))

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
         ((marginalia--buffer-file buffer)))))

    ;; Use my own annotator when listing buffers:
    (map-put! marginalia-annotator-registry 'buffer (list #'my/marginalia-annotate-buffer))

    ;; I don't want any information when listing files:
    (map-delete marginalia-annotator-registry 'file)
    (map-delete marginalia-annotator-registry 'project-file)))

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
    (setq completion-styles '(orderless))

    ;; Restore tramp file completion (recommended by Vertico's README):
    (setq completion-category-overrides '((file (styles basic partial-completion))))))

(use-package xref
  :init
  (progn
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
    (setq xref-search-program 'ripgrep)))

(use-package js
  :mode ("\\.[cm]js\\'" . javascript-mode))

(use-package js2-mode
  :hook (js2-mode . my/js2-mode-reduce-mode-name)
  :config
  (progn
    (defun my/js2-mode-reduce-mode-name ()
      (setq-local mode-name "JS2"))))

(use-package xref-js2
  :init
  (progn
    (setq xref-js2-search-program 'rg)))

(use-package project
  :bind (
         :map project-prefix-map
         ;; Use magit as default command when switching projects:
         ("p" . my/project-switch-project-to-magit)
         ("v" . my/vterm-open-on-project))
  :init
  (progn
    ;; I usually want to consider submodules as different
    ;; projects. This is very useful to add Emacs packages to the list
    ;; of projects. Setting project-vc-merge-submodules to nil in
    ;; ~/.emacs.d/.dir-locals.el wouldn't work as packages usually
    ;; define their own .dir-locals.el, thus overrinding ours.
    (setq-default project-vc-merge-submodules nil)

    (defun my/project-switch-project-to-magit ()
      "Ask the user to select a project from known projects and open magit on the selection."
      (interactive)
      (require 'project)
      (magit-status-setup-buffer (project-prompt-project-dir))))
  :config
  (progn
    ;; Delete commands I don't want to see when switching projects:
    (dolist (undesired-switch-command '(project-vc-dir project-eshell))
      (setq project-switch-commands (cl-delete undesired-switch-command project-switch-commands :key #'car)))

    (add-to-list 'project-switch-commands '(my/vterm-open-new "Shell") t)

    (defconst my/project-root-marker ".project"
      "File indicating the root of a project.")

    (defun my/project-find-root (path)
      "Search up the PATH for `my/project-root-marker'."
      (when-let* ((root (locate-dominating-file path my/project-root-marker)))
        (cons 'transient root)))

    (add-to-list 'project-find-functions #'my/project-find-root)

    (defun my/project-copy-filename (file)
      "Copy the path of FILE relative to the project root to the kill ring."
      (interactive (list (or
                          (buffer-file-name)
                          (and (derived-mode-p 'dired-mode) (dired-filename-at-point))
                          default-directory)))
      (when-let* (file
                  (expanded-filename (expand-file-name file))
                  (root (expand-file-name (project-root
                                           (project-current
                                            nil
                                            (file-name-directory expanded-filename)))))
                  ((string-prefix-p root expanded-filename))
                  (result (substring expanded-filename (length root))))
        (kill-new result)
        (message "%s" result)))))

(use-package consult
  :bind (([remap yank-pop] . consult-yank-replace)
         ;; Virtual Buffers
         ([remap switch-to-buffer] . consult-buffer)
         ;; Grep and Find
         ([remap project-find-regexp] . consult-ripgrep)
         ("C-'" . my/consult-switch-vterm)
         ("C-. C-SPC" . my/consult-mark)
         :map minibuffer-local-map
         ("M-r". consult-history))
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
              (project-root project))))

    (defun my/consult-switch-vterm ()
      "List vterm buffers."
      (interactive)
      (consult-buffer (list vterm-source))))
  :config
  (progn
    ;; Add --hidden to the list of arguments:
    (setq consult-ripgrep-args (concat "rg --hidden " (substring consult-ripgrep-args 3)))

    ;; Configure automatic preview of candidates
    (consult-customize
     consult-ripgrep consult-git-grep consult-grep consult-buffer
     consult-bookmark consult-recent-file consult-xref
     consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
     :preview-key "M-.")

    ;; Remove some sources when listing buffers:
    (dolist (source '(consult--source-bookmark consult--source-project-buffer consult--source-project-file))
      (setq consult-buffer-sources (cl-delete source consult-buffer-sources)))

    (defvar vterm-source
      (list :name     "VTerm"
            :category 'buffer
            :narrow   ?v
            :face     'consult-buffer
            :history  'buffer-name-history
            :state    #'consult--buffer-state
            :new
            (lambda (name)
              (vterm (generate-new-buffer-name name)))
            :items
            (lambda ()
              (mapcar #'buffer-name
                      (seq-filter
                       (lambda (buffer)
                         (eq (buffer-local-value 'major-mode buffer) 'vterm-mode))
                       (buffer-list))))))

    (add-to-list 'consult-buffer-sources 'vterm-source 'append)

    (defun my/consult-mark (globalp)
      "Jump to a marker.
Jump to a global marker unless GLOBALP is nil. Interactively,
jump to a local marker by default and to a global marker if the
prefix arg was used."
      (interactive "P")
      (if globalp
          (consult-global-mark)
        (consult-mark)))))

(use-package consult-flycheck
  :after (consult flycheck)
  :bind (
         :map flycheck-command-map
         ("l" . consult-flycheck)))

(use-package consult-flymake
  :after (consult flymake)
  :bind (
         :map flymake-mode-map
         ("C-c ! l" . consult-flymake)))

(use-package cape
  :demand t
  :after minibuffer
  :bind (("C-M-/" . cape-dabbrev))
  :config
  (progn
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)))

(use-package embark
  :demand t
  :bind (("C-S-a" . embark-act)
         ("C-h b" . embark-bindings)
         ("M-." . embark-dwim)
         :map embark-file-map
         ("v" . my/vterm-open-on-directory)
         :map embark-bookmark-map
         ("c" . compile)
         ("k" . bookmark-delete)
         ("v" . my/vterm-open-on-bookmark)
         ("&" . async-shell-command))
  :init
  (progn
    ;; Pressing C-h after a prefix key lists all suffixes with
    ;; completion:
    (setq prefix-help-command #'embark-prefix-help-command))
  :config
  (progn
    ;; https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt
    (defun my/embark-which-key-indicator ()
      "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
      (lambda (&optional keymap targets prefix)
        (if (null keymap)
            (which-key--hide-popup-ignore-command)
          (which-key--show-keymap
           (if (eq (plist-get (car targets) :type) 'embark-become)
               "Become"
             (format "Act on %s '%s'%s"
                     (plist-get (car targets) :type)
                     (embark--truncate-target (plist-get (car targets) :target))
                     (if (cdr targets) "…" "")))
           (if prefix
               (pcase (lookup-key keymap prefix 'accept-default)
                 ((and (pred keymapp) km) km)
                 (_ (key-binding prefix 'accept-default)))
             keymap)
           nil nil t (lambda (binding)
                       (not (string-suffix-p "-argument" (cdr binding))))))))

    (setq embark-indicators
          '(embark--vertico-indicator
            my/embark-which-key-indicator
            embark-highlight-indicator
            embark-isearch-highlight-indicator))

    ;; Configure `compile':
    (setf (alist-get #'compile embark-target-injection-hooks)
          (list #'embark--allow-edit #'embark--shell-prep))
    (setf (alist-get #'compile embark-around-action-hooks)
          (list #'embark--cd))

    ;; Configure `project-compile':
    (setf (alist-get #'project-compile embark-target-injection-hooks)
          (list #'embark--allow-edit #'embark--shell-prep))

    ;; Configure `async-shell-command':
    (setf (alist-get #'async-shell-command embark-around-action-hooks)
          (list #'embark--cd))))

(use-package embark-consult
  :demand t
  :after (embark consult))

(use-package wgrep
  :demand t
  :after (grep)
  :init
  (progn
    (setq wgrep-enable-key [remap read-only-mode])
    (setq wgrep-auto-save-buffer t)))

(use-package imenu-list
  :bind (("M-i" . my/imenu-list))
  :init
  (progn
    (setq imenu-list-position 'below))
  :config
  (progn
    (defun my/imenu-list (arg)
      "Start `imenu' by default and `imenu-list' with prefix ARG."
      (interactive "P")
      (if arg
          (imenu-list)
        (consult-imenu)))))

(use-package tmr
  :bind (("C-. t" . tmr-prefix-map)
         :map tmr-prefix-map
         ;; prefer "t" over "T" to create a tmr with a description:
         ("t" . tmr-with-details)
         ("T" . nil))
  :init
  (progn
    (setq tmr-description-list '("Check draft PR" "Merge PR" "Check dev-damien"))
    (setq tmr-sound-file
          (expand-file-name (locate-user-emacs-file "media/complete.oga"))))
  :config
  (progn
    (defun my/tmr--acknowledge-prompt ()
      t)

    (advice-add #'tmr--acknowledge-prompt
                :override #'my/tmr--acknowledge-prompt)

    (with-eval-after-load 'embark
      (defvar my/tmr-action-map
        (let ((map (make-sparse-keymap)))
          (define-key map "k" #'tmr-remove)
          (define-key map "K" #'tmr-remove-finished)
          (define-key map "c" #'tmr-clone)
          (define-key map "e" #'tmr-edit-description)
          (define-key map "s" #'tmr-reschedule)
          map))
      (add-to-list 'embark-keymap-alist '(tmr-timer . my/tmr-action-map)))))

(use-package tmr-tabulated
  :bind (
         :map tmr-tabulated-mode-map
         ("a" . tmr-with-description)
         ("K" . tmr-remove-finished)))

(use-package fontaine
  :demand t
  :init
  (progn
    (setq fontaine-presets
          '((medium
             :default-height 110)
            (large
             :default-weight semilight
             :default-height 140
             :bold-weight extrabold)
            (t
             :default-family "Iosevka Comfy"
             :default-weight regular
             :variable-pitch-family "Iosevka Comfy Motion Duo"
             :italic-family "Iosevka Comfy Motion"
             :italic-slant italic))))
  :config
  (progn
    (fontaine-set-preset 'large)))

(use-package spacious-padding
  :demand t
  :hook (server-after-make-frame . spacious-padding-mode))

(use-package ligature
  :demand t
  :config
  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
                          '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                            "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                            "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                            ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  ;; Enable ligature checks globally in all buffers. You can also do
  ;; it per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package racket
  :hook (racket-mode . enable-paredit-mode))

(use-package ob-racket
  :demand t
  :after (org racket-mode))

(use-package meow
  :disabled t
  :config
  (progn
    ;; https://github.com/meow-edit/meow/blob/master/KEYBINDING_COLEMAK.org
    (defun meow-setup ()
      (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak)
      (meow-motion-overwrite-define-key
       ;; Use e to move up, n to move down.
       ;; Since special modes usually use n to move down, we only overwrite e here.
       '("e" . meow-prev)
       '("<escape>" . ignore))
      (meow-leader-define-key
       '("?" . meow-cheatsheet)
       ;; To execute the originally e in MOTION state, use SPC e.
       '("e" . "H-e")
       '("1" . meow-digit-argument)
       '("2" . meow-digit-argument)
       '("3" . meow-digit-argument)
       '("4" . meow-digit-argument)
       '("5" . meow-digit-argument)
       '("6" . meow-digit-argument)
       '("7" . meow-digit-argument)
       '("8" . meow-digit-argument)
       '("9" . meow-digit-argument)
       '("0" . meow-digit-argument))
      (meow-normal-define-key
       '("0" . meow-expand-0)
       '("1" . meow-expand-1)
       '("2" . meow-expand-2)
       '("3" . meow-expand-3)
       '("4" . meow-expand-4)
       '("5" . meow-expand-5)
       '("6" . meow-expand-6)
       '("7" . meow-expand-7)
       '("8" . meow-expand-8)
       '("9" . meow-expand-9)
       '("-" . negative-argument)
       '(";" . meow-reverse)
       '("," . meow-inner-of-thing)
       '("." . meow-bounds-of-thing)
       '("[" . meow-beginning-of-thing)
       '("]" . meow-end-of-thing)
       '("/" . meow-visit)
       '("a" . meow-append)
       '("A" . meow-open-below)
       '("b" . meow-back-word)
       '("B" . meow-back-symbol)
       '("c" . meow-change)
       '("d" . meow-delete)
       '("e" . meow-prev)
       '("E" . meow-prev-expand)
       '("f" . meow-find)
       '("g" . meow-cancel-selection)
       '("G" . meow-grab)
       '("h" . meow-left)
       '("H" . meow-left-expand)
       '("i" . meow-right)
       '("I" . meow-right-expand)
       '("j" . meow-join)
       '("k" . meow-kill)
       '("l" . meow-line)
       '("L" . meow-goto-line)
       '("m" . meow-mark-word)
       '("M" . meow-mark-symbol)
       '("n" . meow-next)
       '("N" . meow-next-expand)
       '("o" . meow-block)
       '("O" . meow-to-block)
       '("p" . meow-yank)
       '("q" . meow-quit)
       '("r" . meow-replace)
       '("s" . meow-insert)
       '("S" . meow-open-above)
       '("t" . meow-till)
       '("u" . meow-undo)
       '("U" . meow-undo-in-selection)
       '("v" . meow-search)
       '("w" . meow-next-word)
       '("W" . meow-next-symbol)
       '("x" . meow-delete)
       '("X" . meow-backward-delete)
       '("y" . meow-save)
       '("z" . meow-pop-selection)
       '("'" . repeat)
       '("<escape>" . ignore)))

    (meow-setup)
    (meow-global-mode 1)))

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

(defun my/add-mark (&rest args)
  (push-mark nil t))

(advice-add #'backward-up-list :before #'my/add-mark)

;; Local Variables:
;; eval: (outline-minor-mode)
;; eval: (flycheck-mode -1)
;; no-byte-compile: t
;; End:
