;;; init.el --- user-init-file                    -*- lexical-binding: t; -*-

(add-to-list 'load-path (locate-user-emacs-file "misc"))

(setq load-prefer-newer t)

;; Apply recommendation from modus Info manual:
(setq face-near-same-color-threshold 45000)

(setq scroll-step 1)
(setq ring-bell-function (lambda ()))

(setq ns-right-alternate-modifier 'none)
(setq ns-right-command-modifier 'none)

;; I don't use package.el to install packages but I still want to
;; configure autoloads and info manuals:
(package-activate-all)

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

(progn ; paragraphs
  (set-default 'sentence-end-double-space nil))

(use-package comp
  :init
  (progn
    (setq native-comp-async-report-warnings-errors nil)))

(use-package auto-compile
  :demand t
  :init
  (progn
    (setq auto-compile-display-buffer nil)
    (setq auto-compile-source-recreate-deletes-dest t)
    (setq auto-compile-toggle-deletes-nonlib-dest t)
    (setq auto-compile-update-autoloads t))
  :hook (auto-compile-inhibit-compile . auto-compile-inhibit-compile-detached-git-head)
  :config
  (progn
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)
    (auto-compile-use-mode-line-set nil nil)))

(use-package no-littering
  :demand t
  :config
  (progn
    ;; no-littering sets the value of `copilot-install-dir' to
    ;; something in ~/.emacs.d/var but I want the default value from
    ;; the `copilot' package instead. This is because the default
    ;; value comes from Nix and points to the installation directory
    ;; of "copilot-node-server". Because `copilot' isn't loaded yet,
    ;; we just delete the variable:
    (makunbound 'copilot-install-dir)))

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
      '(buffer-file-name (:eval (cond
                                 ((not (mode-line-window-selected-p)) "")
                                 (buffer-read-only (format "🔒 "))
                                 ((buffer-modified-p) "💾 ")
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

  (defun my/mode-line-buffer-identification-face ()
    "Return face for `my/mode-line-buffer-identification'."
    (when (mode-line-window-selected-p)
      'mode-line-buffer-id))

  (defvar-local my/mode-line-buffer-identification
      '(:eval (propertize (buffer-name)
                          'face (my/mode-line-buffer-identification-face))))

  (put 'my/mode-line-buffer-identification 'risky-local-variable t)

  (defvar-local my/mode-line-position
      '(:eval (when (mode-line-window-selected-p)
                mode-line-position)))

  (put 'my/mode-line-position 'risky-local-variable t)

  (defvar-local my/mode-line-modes
      '(:eval (when (mode-line-window-selected-p)
                mode-line-modes)))

  (put 'my/mode-line-modes 'risky-local-variable t)

  (defvar-local my/mode-line-misc-info
      '(:eval (when (mode-line-window-selected-p)
                mode-line-misc-info)))

  (put 'my/mode-line-misc-info 'risky-local-variable t)

  ;; Change buffer status to my own function to simplify output:
  (setq-default mode-line-format
                '("%e" ;; error message about full memory
                  "  "
                  my/mode-line-buffer-status
                  my/mode-line-remote
                  my/mode-line-buffer-identification
                  "  "
                  my/mode-line-position
                  "  "
                  my/mode-line-modes
                  my/mode-line-misc-info)))

(use-package which-func
  :defer 5
  :config
  (progn
    (which-function-mode)))

(use-package register
  :config
  (set-register ?t "!-tests.js !.spec.component.js !_spec.ui.js !_spec.e2e.js"))

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

(use-package executable
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package frame
  :config
  (progn
    (defun my/setup-frame (&optional frame)
      "Configure look of FRAME.

If FRAME is nil, configure current frame. If non-nil, make FRAME
current."
      (when frame (select-frame frame))
      (setq frame-title-format "Emacs")
      (modify-all-frames-parameters
       '((cursor-type bar . 5))))

    (my/setup-frame)))

(use-package face-remap
  :bind (("C-x C-+" . global-text-scale-adjust)
         ("C-x C--" . global-text-scale-adjust)
         ("C-x C-=" . global-text-scale-adjust)
         ("C-x C-0" . global-text-scale-adjust)))

(use-package window
  :bind (("C-x o" . nil))
  :init
  (progn
    (setq switch-to-buffer-obey-display-actions t)

    (defun my/window-balance-windows (&rest args)
      "Same as `balance-windows' but ignores arguments."
      (balance-windows))

    (seq-doseq (fn (list #'split-window #'delete-window))
      (advice-add fn :after #'my/window-balance-windows))))

(use-package completion-preview
  :demand t
  :bind (
         :map completion-preview-active-mode-map
         ("M-n" . completion-preview-next-candidate)
         ("M-p" . completion-preview-prev-candidate))
  :config
  (progn
    (global-completion-preview-mode)))

(use-package ace-window
  :demand t
  :bind ("M-o" . ace-window)
  :init
  (progn
    ;; home row in a Colemak layout
    (setq aw-keys '(?a ?r ?s ?t ?n ?e ?i ?o))
    (setq aw-background nil))
  :config
  (progn
    (ace-window-display-mode)
    ;; reduce the height of ace-window letters so they blend nicely
    ;; within their buffers without moving pixels around:
    (set-face-attribute 'aw-leading-char-face nil :height 1.0)))

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

(use-package casual-isearch
  :demand t
  :after isearch
  :bind (:map isearch-mode-map ("C-o" . casual-isearch-tmenu)))

(use-package dabbrev
  :bind (("M-/" . nil)
         ("C-M-/" . nil))
  :init
  (progn
    (setq dabbrev-case-fold-search t)
    (setq dabbrev-case-replace nil))
  :config
  (progn
    ;; recommended by corfu:
    (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
    (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
    (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode)))

(use-package diff
  :bind (
         :map diff-mode-map
         ;; This key is for changing the active window:
         ("M-o" . nil)))

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
      (defun my/pdf-tools-backdrop (&rest _)
        (modus-themes-with-colors
          (face-remap-add-relative
           'default
           `(:background ,bg-dim))))

      (add-hook 'pdf-tools-enabled-hook #'my/pdf-tools-backdrop)

      ;; Configure faces for combobulate that is not yet supported by
      ;; modus:
      (modus-themes-with-colors
        (custom-set-faces
         `(combobulate-active-indicator-face ((,c :foreground ,fg-main)))
         `(combobulate-dimmed-indicator-face ((,c :inherit shadow)))
         `(combobulate-error-indicator-face ((,c :inherit error)))
         `(combobulate-query-highlight-fiery-flames-face ((,c :inherit
                                                              modus-themes-intense-red)))
         `(combobulate-query-highlight-gleaming-gold-face ((,c :inherit
                                                               modus-themes-intense-yellow)))
         `(combobulate-query-highlight-majestic-mercury-face ((,c :inherit
                                                                  modus-themes-intense-cyan)))
         `(combobulate-query-highlight-mysterious-mauve-face ((,c :inherit
                                                                  modus-themes-intense-magenta)))
         `(combobulate-query-highlight-radiant-rind-face ((,c :inherit
                                                              modus-themes-subtle-red)))
         `(combobulate-query-highlight-regal-ripples-face ((,c :inherit
                                                               modus-themes-intense-blue)))
         `(combobulate-query-highlight-serene-shade-face ((,c :inherit
                                                              modus-themes-subtle-green)))
         `(combobulate-query-highlight-silver-shadows-face ((,c :background ,bg-active
                                                                :foreground ,fg-main)))
         `(combobulate-query-highlight-vibrant-veggie-face ((,c :inherit
                                                                modus-themes-intense-green)))
         `(combobulate-query-query-anonymous-face ((,c :inherit modus-themes-bold
                                                       :foreground ,fg-alt)))
         `(combobulate-query-query-builtin-face ((,c :inherit font-lock-builtin-face)))
         `(combobulate-query-query-constant-face ((,c :inherit font-lock-constant-face)))
         `(combobulate-query-query-doc-markup-face ((,c :inherit
                                                        font-lock-doc-markup-face)))
         `(combobulate-query-query-keyword-face ((,c :inherit font-lock-keyword-face)))
         `(combobulate-query-query-predicate-builtin-face ((,c :inherit bold)))
         `(combobulate-query-query-string-face ((,c :inherit font-lock-string-face)))
         `(combobulate-refactor-choice-face ((,c :inherit modus-themes-slant :foreground
                                                 ,info)))
         `(combobulate-refactor-cursor-face ((,c :foreground ,cursor)))
         `(combobulate-refactor-field-face ((,c :background ,bg-inactive :foreground
                                                ,fg-main :extend nil)))
         `(combobulate-refactor-highlight-face ((,c :inherit highlight)))
         `(combobulate-refactor-inactive-choice-face ((,c :inherit modus-themes-slant
                                                          :foreground ,fg-dim)))
         `(combobulate-refactor-inactive-field-face ((,c :background ,bg-dim :foreground
                                                         ,fg-dim :extend nil)))
         `(combobulate-refactor-label-face ((,c :inherit modus-themes-search-replace)))
         `(combobulate-tree-branch-face ((,c :inherit shadow)))
         `(combobulate-tree-highlighted-node-face ((,c :inherit success)))
         `(combobulate-tree-normal-node-face ((,c :foreground ,fg-main)))
         `(combobulate-tree-pulse-node-face ((,c :background ,bg-blue-intense :extend t))))))))

(use-package tramp
  :config
  (progn
    (add-to-list 'tramp-remote-path "/run/current-system/profile/bin")))

(use-package simple
  :demand t
  :bind (("M-j" . my/join-line)
         ("C-x a" . beginning-of-buffer)
         ("M-<" . nil)
         ("C-x e" . end-of-buffer)
         ("M->" . nil)
         ([remap yank-pop] . yank-from-kill-ring)
         ("C-_" . nil) ;; force me to use C-/ to undo
         ("C-. s" . scratch-buffer)
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
  :hook ((emacs-lisp-mode . my/elisp-mode-reduce-mode-name)
         (emacs-lisp-mode . my/eldoc-shows-more-information))
  :config
  (progn
    (defun my/elisp-mode-reduce-mode-name ()
      (setq-local mode-name "Elisp"))

    (defun my/eldoc-shows-more-information ()
      (remove-hook 'eldoc-documentation-functions #'elisp-eldoc-var-docstring t)
      (add-hook 'eldoc-documentation-functions #'elisp-eldoc-var-docstring-with-value nil t))))

(use-package minibuffer
  :bind (("M-/" . completion-at-point))
  :init
  (progn
    (setq read-file-name-completion-ignore-case t)
    (setq completions-detailed t))
  :config
  (progn
    (dolist (regexp '("File .* removed from the recentf list"))
      (add-to-list 'inhibit-message-regexps regexp nil #'string=))
    (add-to-list 'set-message-functions #'set-multi-message)
    (add-to-list 'set-message-functions #'inhibit-message)))

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

(use-package apropos
  :hook (apropos-mode . outline-minor-mode))

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
    (setq proced-filter 'all)
    (setq proced-enable-color-flag t)))

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
  :hook (((prog-mode magit-process-mode) . goto-address-mode))
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
    (setq dired-mouse-drag-files t)

    (defun my/dired-move-beginning-of-line ()
      (interactive)
      (let ((point (point)))
        (dired-move-to-filename)
        (when (= point (point))
          (move-beginning-of-line nil))))))

(use-package casual-dired
  :after dired
  :bind (
         :map dired-mode-map
         ("?" . casual-dired-tmenu)
         ("s" . casual-dired-sort-by-tmenu)))

(use-package casual-re-builder
  :after re-builder
  :bind (:map
         reb-mode-map ("C-o" . casual-re-builder-tmenu)
         :map
         reb-lisp-mode-map ("C-o" . casual-re-builder-tmenu)))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

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
    (setq recentf-max-saved-items 1000)
    ;; recommended by consult in `consult--source-recent-file':
    (setq recentf-filename-handlers nil))
  :config
  (progn
    (recentf-mode)))

(use-package magit
  :bind ((
          :map magit-mode-map
          ("M-w" . magit-copy-section-value)))
  :init
  (progn
    (setq magit-show-long-lines-warning nil)
    (setq magit-diff-refine-hunk t)
    (setq magit-branch-prefer-remote-upstream '("master"))
    (setq magit-branch-adjust-remote-upstream-alist '(("origin/master" "master")
                                                      ("origin/main" "main")))
    (setq magit-module-sections-nested nil)
    (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
    (setq magit-no-confirm '(amend-published trash delete-unmerged-branch))
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
      '("x" "Absorb changes" magit-commit-absorb))

    (dir-locals-set-class-variables 'my/magit-huge-git-repository
                                    '((magit-status-mode
                                       .
                                       ((eval . (magit-disable-section-inserter 'magit-insert-tags-header))
                                        (eval . (magit-disable-section-inserter 'magit-insert-untracked-files))
                                        (eval . (magit-disable-section-inserter 'magit-insert-modules))))))

    (let ((huge-repos
           '("~/Documents/projects/nix/nixpkgs-master"
             "~/Documents/projects/nix-system/nixpkgs/")))
      (dolist (repo huge-repos)
        (dir-locals-set-directory-class
         (expand-file-name repo)
         'my/magit-huge-git-repository)))))

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
                              ((string-match-p "foretagsplatsen/monitor" url) 15)
                              ((string-match-p "foretagsplatsen/frontend" url) 20)
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
                 (list 'js2-mode 'js2-jsdoc-value 'js2-jsdoc-type))

    ;; https://github.com/minad/jinx/wiki
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

    ;; https://github.com/minad/jinx/wiki
    (defun my/jinx-add-ispell-localwords ()
      "Add ispell's local words to `jinx-local-words'."
      (let ((ispell-localwords (my/jinx-ispell-localwords)))
        (setq jinx-local-words (concat jinx-local-words ispell-localwords))
        (setq jinx--session-words (append jinx--session-words (split-string ispell-localwords)))))

    ;; https://github.com/minad/jinx/wiki
    (defun my/jinx-save-as-ispell-localword (save key word)
      "Save WORD using ispell's `ispell-words-keyword'.
If SAVE is non-nil save, otherwise format candidate given action KEY."
      (if save
          (progn
            (require 'ispell)
            (ispell-add-per-file-word-list word)
            (add-to-list 'jinx--session-words word)
            (setq jinx-local-words
                  (string-join
                   (sort (delete-dups
                          (cons word (split-string jinx-local-words)))
                         #'string<)
                   " ")))
        (list key word "File")))

    (require 'map)
    (map-put! jinx--save-keys ?* #'my/jinx-save-as-ispell-localword)))

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

(use-package ledger-mode
  :hook (ledger-mode . my/configure-ledger-mode)
  :mode "\\.hledger\\'"
  :bind (
         :map ledger-mode-map
         ("C-c C-r" . ledger-report)
         ("C-c C-c" . my/ledger-lint)
         ("M-q" . my/ledger-reindent)
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
           '(("Account statement" . "register --ignore-assertions --auto ^%(account)")
             ("Income statement"  . "balance --ignore-assertions --auto --tree --period %(month) --invert ^income ^expense")
             ("Balance sheet"     . "balance --ignore-assertions --auto --tree ^asset ^debt \"^equity:\"")
             ("Budget"            . "balance --ignore-assertions --auto --tree --empty ^budget not:unbudgeted"))))

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
    (defun my/ledger-reindent ()
      "Fill the current transaction and reindent it."
      (interactive)
      (with-demoted-errors (ledger-post-fill))
      (ledger-post-align-dwim))

    (let ((date-format "%A, %B %-e"))
      (defun my/ledger-position-at-date (moment)
        "Move point in current buffer to insert new transaction at MOMENT.
MOMENT is an encoded date."
        (let ((heading (format "*** %s" (format-time-string date-format moment))))
          (goto-char (point-min))
          (search-forward heading)
          (forward-line)
          (re-search-forward "; \\*\\*" nil t)
          (goto-char (line-beginning-position)))))

    (advice-add #'ledger-xact-find-slot :override #'my/ledger-position-at-date)

    (defun my/ledger-lint ()
      "Lint my ledger file."
      (interactive)
      (require 'autoclose-shell)
      (save-buffer)
      (autoclose-shell-start "lint-system" '("lint-system")))

    (defun my/ledger-insert-mortgage-transaction ()
      "Read accounting data for MORTGAGE and write the ledger entry."
      (interactive)
      (let* ((description (read-string "Description: "))
             (date (ledger-read-date "Date: ")))
        (my/ledger-position-at-date (ledger-parse-iso-date date))
        (insert (format "%s %s\n" date description))
        (insert (format "  asset:current:couple  0\n"))
        (insert (format "  expense:misc  0\n"))
        (save-excursion
          (my/ledger-mortgage-rewrite))))

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
       ((= (map-elt numbers :interest) 0) 'ecoptz)
       ((>= (map-elt numbers :capital) 410) 'immo1)
       (t 'immo2)))

    (defun my/ledger-mortgage-rewrite ()
      "Rewrite the mortgage transaction at point."
      (interactive)
      (when-let* ((numbers (my/ledger-mortgage-read-numbers))
                  (total (seq-reduce
                          #'+
                          (map-values (my/ledger-mortgage-read-numbers))
                          0))
                  (mortgage-type (my/ledger-mortgage-guess-type numbers)))
        (save-match-data
          (save-excursion
            (ledger-navigate-beginning-of-xact)
            (when (re-search-forward " .*$" (line-end-position)) ; skip date
              (replace-match " banque populaire prêt" t)
              (next-line)
              (delete-region (line-beginning-position) (line-end-position))
              (insert (format " asset:current:couple  %.2f" (- total)))
              (ledger-navigate-end-of-xact)
              (delete-region (line-beginning-position) (line-end-position))
              (map-do
               (lambda (number-type number)
                 (when (> number 0)
                   (insert
                    (format
                     " expense:mortgage:%s%s  %s\n"
                     mortgage-type number-type number))))
               numbers)
              (delete-backward-char 1) ; remove additional newline
              (ledger-post-align-dwim))))))))

(use-package flymake-hledger
  :config
  (progn
    (setq flymake-hledger-command '("hledger" "--auto"))

    ;; Enable 4 optional checks. See URL
    ;; https://hledger.org/1.30/hledger.html#check for the meaning of
    ;; each check and a list of all of them.
    (dolist (check '("ordereddates" "payees" "recentassertions" "tags"))
      (add-to-list 'flymake-hledger-checks check))))

(use-package package-lint
  :after (flymake)
  :hook (emacs-lisp-mode . my/package-lint-flymake-setup)
  :config
  (progn
    (defun my/package-lint-flymake-setup ()
      (add-hook 'flymake-diagnostic-functions #'package-lint-flymake nil t))))

(use-package package-lint-flymake
  :after (flymake package-lint)
  :demand t)

(use-package ledger-complete
  :init
  (progn
    (setq ledger-complete-in-steps nil)))

(use-package ledger-import
  :hook ((ledger-import-finished . my/ledger-import-finish))
  :config
  (progn
    (setq ledger-import-boobank-import-from-date "2024-05-16")
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
    (setq org-directory "~/personal/notes/org")
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
    (setq org-reveal-reveal-js-version 4)))

(use-package ox-icalendar ;; used by org-caldav
  :init
  (progn
    (setq org-icalendar-alarm-time 15)))

(use-package org-caldav
  :bind (("C-. o S"   . org-caldav-sync))
  :config
  (progn
    (setq org-caldav-url "https://licorne.ninja/remote.php/dav/calendars/DamienCassou"
          org-caldav-calendar-id "personal"
          org-caldav-inbox org-default-calendar-file
          org-caldav-files '()
          org-icalendar-timezone "Europe/Berlin"
          org-caldav-sync-changes-to-org 'all)

    (defun my/org-caldav-archive-year (year)
      "Archive a given year in my calendar."
      (interactive "nWhich year would you like to archive? ")
      (goto-char (point-min))
      (while (re-search-forward (format "^ *<%s-.*>$" year) nil t)
        (org-archive-subtree)))))

(use-package denote
  :bind (("C-. r t" . my/denote-today)
         ("C-. r d" . my/denote-date)
         ("C-. r n" . denote)
         ("C-. r f" . my/denote-find-file)
         ("C-. r g" . my/denote-grep))
  :hook (dired-mode . denote-dired-mode-in-directories)
  :init
  (progn
    (setq denote-date-prompt-use-org-read-date t)
    (setq denote-directory (expand-file-name "~/personal/notes/denote"))
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
      (interactive (list (denote-file-prompt (rx ".org" eos))))
      (find-file filename))

    (defun my/denote-today ()
      "Open note for today and create it if necessary."
      (interactive)
      (my/denote-date nil))

    (defun my/denote-date (&optional date)
      "Open note for DATE and create it if necessary.

Interactively, DATE is asked to the user."
      (interactive (list (and
                          (require 'org)
                          (encode-time
                           (iso8601-parse (concat (org-read-date) "T00:00:00"))))))
      (let* ((title (format-time-string "%A %e %B %Y" date))
             (sluggified-title (denote-sluggify 'title title))
             (all-files (denote-directory-files (rx ".org" eos)))
             (matching-files (seq-filter
                              (apply-partially
                               #'string-match-p
                               (regexp-quote sluggified-title))
                              all-files)))
        (cond
         ((length= matching-files 0) (denote title '("journal")))
         ((length= matching-files 1) (find-file (car matching-files)) (goto-char (point-max)))
         (t (user-error "Several notes in '%s' match '%s'" (denote-directory) sluggified-title)))))

    (defun my/denote-grep ()
      "Search within my notes."
      (interactive)
      (consult-ripgrep denote-directory))

    (defun my/denote-attach (file &optional description)
      "Save FILE in attachments/ directory and add a link in current buffer.
The link will contain DESCRIPTION as text."
      (interactive "*fSelect file to attach: \nMDescription: " org-mode)
      (let ((target-dir (expand-file-name "attachments" denote-directory)))
        (unless (file-directory-p target-dir)
          (make-directory target-dir))
        (let* ((target-basename (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
               (target-filename (make-temp-file
                                 (expand-file-name (concat target-basename ".") target-dir)
                                 nil
                                 (concat "." (file-name-extension file)))))
          (copy-file file target-filename t)
          (org-insert-link nil (concat "file:" target-filename) description)
          (when (yes-or-no-p "Delete the initial file? ")
            (delete-file file t)))))))

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
  :bind (:map drag-stuff-mode-map
              ("M-P" . drag-stuff-up)
              ("M-N" . drag-stuff-down))
  :hook ((nix-ts-mode text-mode conf-mode) . drag-stuff-mode))

(use-package expand-region
  :bind ("M-h" . er/expand-region))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

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
  :init
  (progn
    (setq mml-attach-file-at-the-end t)))

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
  :demand t
  :hook (prog-mode . combobulate-mode))

(use-package elec-pair
  :demand t
  :config
  (progn
    (electric-pair-mode)))

(use-package paren
  :demand t
  :init
  (progn
    (setq show-paren-context-when-offscreen t))
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
    (setq describe-bindings-outline t)
    (setq help-window-select t)))

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

    (defun my/avy-action-comment-line (point)
      "Comment the whole line at POINT."
      (my/avy-without-moving-point point
        (comment-line 1)))

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
            (?\M-\; . my/avy-action-comment-line)
            (?\C-\S-a . my/avy-action-embark)))))

(use-package beginend
  :demand t
  :config
  (progn
    (beginend-global-mode)))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init
  (progn
    (setq markdown-gfm-additional-languages '("bash"))))

(use-package repeat
  :demand t
  :bind (("C-z" . repeat))
  :config
  (progn
    (repeat-mode)))

(use-package khardel
  :bind (("C-. c" . khardel-insert-email)))

(use-package nameless
  :hook (emacs-lisp-mode . nameless-mode)
  :init
  (progn
    (setq nameless-affect-indentation-and-filling nil)
    (setq nameless-prefix "…")))

(use-package eww
  :init
  (progn
    (setq eww-auto-rename-buffer 'title)))

(use-package epithet
  :hook (((Info-selection help-mode occur shell-mode)
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
  :hook (prodigy-mode . my/prodigy-setup)
  :bind (("C-. p" . prodigy)
         :map prodigy-mode-map
         ("k" . (lambda () (interactive) (prodigy-stop t)))
         ("M-w" . prodigy-copy-url)
         ("C-<down>" . prodigy-next-with-status)
         ("C-<up>" . prodigy-prev-with-status))
  :init
  (progn
    (setq prodigy-completion-system 'default))
  :config
  (progn
    (defun my/prodigy-setup ()
      "Configure prodigy."
      (setq-local browse-url-browser-function #'browse-url-chromium))))

(use-package libbcel
  :config
  (progn
    (setq libbcel-oauth-store-encryption-keys (list "8E64FBE545A394F5D35CD202F72C652AE7564ECC"))
    (setq libbcel-oauth-client-id (auth-source-pass-get "client_id" "ftgp/37signals.com"))
    (setq libbcel-oauth-client-secret (auth-source-pass-get "client_secret" "ftgp/37signals.com"))
    (setq libbcel-client-account-id (auth-source-pass-get "account_id" "ftgp/37signals.com"))))

(use-package libelcouch
  :init
  (progn
    (setq libelcouch-timeout 100)
    (setq libelcouch-couchdb-instances '(("Local" "http://localhost:5984")))))

(use-package related-files
  :bind (("C-x j" . related-files-jump)
         ("C-x J" . related-files-make)))

(use-package related-files-recipe
  :demand t
  :after related-files)

(use-package flymake-eslint
  :init
  (progn
    (setq flymake-eslint-executable-name "eslint_d")))

(use-package reformatter
  :demand t
  :commands (prettier-on-save-mode)
  :config
  (progn
    (reformatter-define prettier
      :program (let* ((root (locate-dominating-file default-directory "node_modules/.bin/prettier"))
                      (exec-path (if root
                                     (cons (expand-file-name "node_modules/.bin" root) exec-path)
                                   exec-path)))
                 (executable-find "prettier"))
      :args (list "--stdin-filepath" (buffer-file-name))
      :input-file (reformatter-temp-file-in-current-directory))))

(use-package eslint-disable-rule
  :demand t
  :after (:all js2-mode (:any flymake flycheck))
  :init
  (progn
    (setq eslint-disable-rule-require-description 'prefer-description))
  :config
  (progn
    (with-eval-after-load 'flymake
      (bind-key "C-c ! k" #'eslint-disable-rule-disable-next-line flymake-mode-map))

    (with-eval-after-load 'flycheck
      (bind-key "C-c ! k" #'eslint-disable-rule-disable-next-line flycheck-mode-map))))

(use-package finsit-fill-pr-description-template
  :after forge
  :hook ((forge-create-pullreq . finsit-fill-pr-description-template)))

(use-package finsit-insert-commit-id
  :after git-commit
  :hook (git-commit-setup . finsit-insert-commit-id))

(use-package finsit-js-add-import
  :init
  (progn
    (with-eval-after-load 'embark
      (defvar embark-file-map)
      (define-key embark-file-map "z" #'finsit-js-add-import))))

(use-package finsit-js-test-structure
  :hook ((js2-mode . finsit-js-test-structure-imenu-mode)))

(use-package finsit-write-pr-to-basecamp
  :after forge
  :hook ((forge-post-submit-callback . finsit-write-pr-to-basecamp)))

(use-package finsit-prodigy
  :demand t
  :after prodigy
  :config
  (progn
    (finsit-prodigy-setup
     ;; path to the frontend:
     (expand-file-name "~/work/setup/frontend")
     ;; path to the backend:
     (expand-file-name "~/work/setup/monitor"))))

(use-package alert
  :demand t
  :init
  (progn
    (setq alert-default-style (if (eq 'darwin system-type)
                                  'osx-notifier
                                'notifications))))

(use-package diff-hl-dired
  :demand t
  :after diff-hl
  :hook ((dired-mode . diff-hl-dired-mode-unless-remote)))

(use-package yasnippet
  :hook (((org-mode git-commit-mode css-mode prog-mode) . yas-minor-mode))
  :config
  (progn
    (add-to-list 'yas-snippet-dirs (locate-user-emacs-file "lib/ftgp/snippets"))
    (yas-reload-all)))

(use-package ws-butler
  :hook ((prog-mode ledger-mode) . ws-butler-mode))

(use-package editorconfig
  :hook ((prog-mode text-mode) . editorconfig-mode))

(use-package compile
  :hook ((compilation-filter . ansi-color-compilation-filter)
         (compilation-filter . ansi-osc-compilation-filter)))

(use-package misc
  :bind (("M-D" . duplicate-dwim)))

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

(use-package vterm
  :commands (vterm)
  :bind (
         ("C-M-'" . my/vterm-open-new)
         :map vterm-mode-map
         ("<f8>" . vterm-send-C-x)
         ("C-<up>" . vterm-previous-prompt)
         ("C-<down>" . vterm-next-prompt))
  :hook (vterm-mode . my/vterm-setup)
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
        (my/vterm-open-new)))

    (defun my/vterm-setup ()
      ;; On darwin, the .profile is not loaded by the system on
      ;; startup, terminals have to do it manually:
      (when (memq system-type '(darwin))
        (vterm-send-string "source ~/.profile" t)
        (vterm-send-return)))))

(use-package docker
  :bind (("C-. d" . docker)))

(use-package dockerfile-ts-mode
  :mode "\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'")

(use-package yaml-ts-mode
  :mode "\\.ya?ml\\'" )

(use-package json-ts-mode
  :mode "\\.json\\'")

(use-package toml-ts-mode
  :mode "\\.toml\\'")

(use-package json-mode
  :config
  (progn
    ;; Remove automatically-added entries:
    (setq magic-fallback-mode-alist
          (cl-delete 'json-mode magic-fallback-mode-alist :key #'cdr))
    (setq auto-mode-alist
          (cl-delete 'json-mode auto-mode-alist :key #'cdr))))

(use-package sh-script
  :interpreter ("bash" . bash-ts-mode)
  :mode ("\\.\\(?:bash\\(?:_\\(?:history\\|profile\\)\\|rc\\)\\|profile\\)\\'" . bash-ts-mode))

(use-package csharp-mode
  :mode ("\\.cs\\'" . csharp-ts-mode))

(use-package eglot
  :hook ((eglot-managed-mode . my/eglot-setup)
         ((bash-ts-mode css-ts-mode yaml-ts-mode dockerfile-ts-mode graphviz-dot-mode json-ts-mode go-ts-mode) . my/eglot-ensure))
  :commands (my/eglot-format-on-save-mode)
  :config
  (progn
    (add-to-list 'eglot-server-programs
                 '(nix-ts-mode
                   .
                   ("nil"
                    :initializationOptions (:nil (:formatting (:command ["nixfmt"]))))))

    ;; The above `add-to-list' should be enough but it doesn't seem to work
    ;; https://github.com/oxalica/nil/issues/101
    (setq-default eglot-workspace-configuration
                  (append eglot-workspace-configuration '(:nil (:formatting (:command ["nixfmt"])))))

    (defun my/eglot-ensure ()
      "Only start eglot in file-visiting buffers."
      (when (buffer-file-name)
        (eglot-ensure)))

    (defun my/eglot-setup ()
      "Misc changes to eglot's configuration."
      ;; Stop eglot from overriding `eldoc-documentation-strategy':
      (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
      ;; Re-enable flymake-eslint if it was active:
      (when (seq-contains-p (map-elt eglot--saved-bindings 'flymake-diagnostic-functions) 'flymake-eslint--checker)
        (setq flymake-diagnostic-functions (list 'flymake-eslint--checker))))

    (define-minor-mode my/eglot-format-on-save-mode
      "Automatically format buffer with eglot before saving."
      :lighter ""
      (if my/eglot-format-on-save-mode
          (add-hook 'before-save-hook #'my/eglot-format-buffer nil t)
        (remove-hook 'before-save-hook #'my/eglot-format-buffer t)))

    (defun my/eglot-format-buffer ()
      "Use eglot to format the buffer if eglot is active."
      (interactive)
      (when (and (eglot-managed-p) (eglot--server-capable :documentFormattingProvider))
        (eglot-format-buffer)))))

(use-package dape
  :init
  (progn
    (setq dape-buffer-window-arrangement 'right)
    (setq dape-debug t))
  :config
  (progn
    (remove-hook 'dape-on-start-hooks 'dape-info)
    (remove-hook 'dape-on-start-hooks 'dape-repl)))

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
    (setq libmpdel-music-directory (getenv "XDG_MUSIC_DIR")))
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
    (setq-default my/mode-line-modes
                  (cl-subst 'minions-mode-line-modes
                            'mode-line-modes
                            (default-value 'my/mode-line-modes)))))

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

(use-package nix-mode
  :config
  (progn
    (setq auto-mode-alist
          (cl-delete 'nix-mode auto-mode-alist :key #'cdr))))

(use-package nix-prettify-mode
  :hook ((proced-mode . nix-prettify-mode)))

(use-package nix-ts-mode
  :mode ("\\.nix\\'" . nix-ts-mode))

(use-package html-ts-mode
  :mode ("\\.html\\'" . html-ts-mode))

(use-package css-ts-mode
  :mode ("\\.css\\'" . css-ts-mode))

(use-package typescript-mode
  :config
  (progn
    (setq auto-mode-alist
          (cl-delete 'typescript-mode auto-mode-alist :key #'cdr))))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

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
            (consult-ripgrep buffer)))

    (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))))

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
    (require 'map)
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
    (setq completion-styles '(orderless basic))))

(use-package orderless-kwd
  :demand t
  :after orderless
  :config
  (progn
    ;; allow filtering completion lists with keywords
    (add-to-list 'orderless-style-dispatchers #'orderless-kwd-dispatch)))

(use-package xref
  :hook (xref-after-update . outline-minor-mode)
  :init
  (progn
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
    (setq xref-search-program 'ripgrep)))

(use-package js
  :mode (("\\.[cm]?js\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode))
  :interpreter ("node" . js-ts-mode)
  :bind (
         :map js-ts-mode-map
         ("M-." . nil))
  :config
  (progn
    (setq auto-mode-alist
          (cl-delete 'javascript-mode auto-mode-alist :key #'cdr))

    (setq interpreter-mode-alist
          (cl-delete 'js-mode interpreter-mode-alist :key #'cdr))))

(use-package js2-mode
  :hook (js-base-mode . js2-minor-mode)
  :config
  (progn
    (setq js2-mode-show-strict-warnings nil)
    (setq js2-mode-show-parse-errors nil)))

(use-package js2-refactor
  :hook ((js2-mode js2-minor-mode) . js2-refactor-mode)
  :config
  (progn
    (js2r-add-keybindings-with-prefix "C-c C-r")))

(use-package xref-js2
  :demand t
  :after (js2-mode xref)
  :hook (js2-minor-mode . my/xref-js2-setup)
  :config
  (progn
    (setq xref-js2-search-program 'rg)

    (defun my/xref-js2-setup ()
      "Configure xref-js2."
      (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package rjsx-mode
  :config
  (progn
    (setq auto-mode-alist
          (cl-delete 'rjsx-mode auto-mode-alist :key #'cdr))))

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
    ;; define their own .dir-locals.el, thus overriding ours.
    (setq-default project-vc-merge-submodules nil)

    (defun my/project-switch-project-to-magit ()
      "Ask the user to select a project from known projects and open magit on the selection."
      (interactive)
      (require 'project)
      (let ((project-directory (project-prompt-project-dir)))
        (if (magit-git-repo-p project-directory)
            (magit-status-setup-buffer project-directory)
          (dired project-directory)))))
  :config
  (progn
    ;; Delete commands I don't want to see when switching projects:
    (dolist (undesired-switch-command '(project-vc-dir project-eshell))
      (setq project-switch-commands (cl-delete undesired-switch-command project-switch-commands :key #'car)))

    (add-to-list 'project-switch-commands '(my/vterm-open-new "Shell") t)

    (add-to-list 'project-vc-extra-root-markers "package.json")
    (add-to-list 'project-vc-extra-root-markers "*.sln")

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

(use-package conner
  :bind (([remap project-compile] . conner-run-project-command))
  :config
  (progn
    ;; make sure to load vterm because conner checks for it:
    (require 'vterm)))

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
    ;; Configure automatic preview of candidates
    (consult-customize
     consult-ripgrep consult-git-grep consult-grep consult-buffer
     consult-bookmark consult-recent-file consult-xref
     consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
     :preview-key "M-.")

    ;; Remove some sources when listing buffers:
    (dolist (source '(consult--source-project-buffer consult--source-project-file))
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
    (setq embark-confirm-act-all nil)
    (setq embark-indicators
          '(embark-minimal-indicator
            embark--vertico-indicator
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

    (defun my/tmr--notification-notify (timer)
      "Replaces `tmr-notification-notify' to use the alert package.
The alert package works on different platforms."
      (let ((title "TMR")
            (body (tmr--long-description-for-finished-timer timer)))
        (alert body :title title)))

    (advice-add #'tmr-notification-notify
                :override #'my/tmr--notification-notify)

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
             :default-height 130
             :bold-weight extrabold)
            (t
             :default-family "Aporetic sans mono"
             :default-weight regular
             :variable-pitch-family "Aporetic serif"
             :italic-family "Aporetic sans mono"
             :italic-slant italic))))
  :config
  (progn
    (fontaine-set-preset 'large)))

(use-package spacious-padding
  :demand t
  :hook (server-after-make-frame . spacious-padding-mode)
  :config
  (progn
    (setq spacious-padding-subtle-mode-line
          '(:mode-line-active error :mode-line-inactive shadow))))

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

(use-package dotenv-mode
  :mode (("\\.env\\'" . dotenv-mode)
         ("\\.env\\.local\\'" . dotenv-mode)
         ("\\.env\\.development\\'" . dotenv-mode)))

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

;; https://emacsnotes.wordpress.com/2023/09/14/view-emacs-news-files-as-info-manual-too/
(defun my/view-text-file-as-info-manual ()
  "View ‘info’, ‘texi’, ‘org’, ‘md’ and 'NEWS' files as ‘Info’ manual."
  (interactive)
  (require 'rx)
  (require 'ox-texinfo)
  (when (buffer-file-name)
    (let* ((org-export-with-broken-links 'mark)
           (ext (file-name-extension (buffer-file-name))))
      (cond
       ;; A `.info' file
       ((or (string= "info" ext))
        (info (buffer-file-name)))
       ;; A `.texi' file
       ((or (string= "texi" ext))
        (info (org-texinfo-compile (buffer-file-name))))
       ;; An `.org' file
       ((or (derived-mode-p 'org-mode)
            (string= "org" ext))
        (info (org-texinfo-export-to-info)))
       ;; A `.md' file
       ((or (derived-mode-p 'markdown-mode)
            (string= "md" ext))
        (let ((org-file-name (concat (file-name-sans-extension (buffer-file-name)) ".org")))
          (apply #'call-process "pandoc" nil standard-output nil
                 `("-f" "markdown"
                   "-t" "org"
                   "-o" ,org-file-name
                   ,(buffer-file-name)))
          (with-current-buffer (find-file-noselect org-file-name)
            (info (org-texinfo-export-to-info)))))
       (t (user-error "Don't know how to convert `%s' to an `info' file"
                      (buffer-file-name)))))))

(global-set-key (kbd "C-x x v") 'my/view-text-file-as-info-manual)

;; Local Variables:
;; eval: (outline-minor-mode)
;; eval: (flycheck-mode -1)
;; no-byte-compile: t
;; End:
