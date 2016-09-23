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
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(beacon-blink-when-focused t)
 '(bookmark-save-flag 1)
 '(calendar-date-style (quote european))
 '(calendar-week-start-day 1)
 '(checkdoc-spellcheck-documentation-flag t)
 '(column-number-mode t)
 '(confirm-kill-emacs (quote y-or-n-p))
 '(counsel-find-file-at-point t)
 '(current-language-environment "UTF-8")
 '(custom-safe-themes t)
 '(delete-active-region nil)
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(diff-switches "-u")
 '(dired-dwim-target t)
 '(dired-listing-switches "-alh")
 '(dired-recursive-deletes (quote always))
 '(display-time-24hr-format t)
 '(display-time-default-load-average nil)
 '(eclim-executable "~/usr/eclipse.indigo-eclim/eclim")
 '(eclim-print-debug-messages t)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(enable-local-variables :all)
 '(enable-recursive-minibuffers t)
 '(eshell-buffer-shorthand t)
 '(eshell-cmpl-cycle-completions nil)
 '(eval-expression-print-length 20)
 '(eval-expression-print-level nil)
 '(flyspell-use-meta-tab nil)
 '(frame-title-format "Emacs: %b" t)
 '(gc-cons-threshold 20000000)
 '(global-font-lock-mode t)
 '(global-hl-line-mode t)
 '(global-hl-line-sticky-flag t)
 '(global-pair-mode t)
 '(global-prettify-symbols-mode t)
 '(global-undo-tree-mode t)
 '(haskell-hoogle-command "hoogle")
 '(imenu-auto-rescan t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(initial-major-mode (quote text-mode))
 '(initial-scratch-message nil)
 '(load-prefer-newer t)
 '(magit-diff-refine-hunk nil)
 '(magit-process-find-password-functions (quote (magit-process-password-auth-source)))
 '(magit-repository-directories
   (quote
    ("~/Documents/writing" "~/Documents/candidatures" "~/Documents" "~/.emacs.d/packages" "~/.emacs.d/themes" "~/Documents/smalltalk" "~/tmp/emacs-configurations" "~/tmp" "~/Documents/projects" "~/Documents/websites" "~/Documents/teaching" "~/")))
 '(magit-repository-directories-depth 1)
 '(magit-revert-buffers t t)
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
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/places")
 '(scheme-program-name "petite")
 '(scroll-bar-mode nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(sentence-end-double-space nil)
 '(sh-indent-comment t)
 '(shell-switcher-ask-before-creating-new t)
 '(shell-switcher-mode t)
 '(shell-switcher-new-shell-function (quote shell-switcher-make-eshell))
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
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
 '(svn-status-hide-unmodified t)
 '(svn-status-prefix-key [(control x) 118])
 '(svn-status-verbose t)
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
 '(use-package-enable-imenu-support t)
 '(user-full-name "Damien Cassou")
 '(user-mail-address "damien.cassou@gmail.com")
 '(vc-follow-symlinks nil)
 '(vdirel-repository "~/Documents/configuration/contacts")
 '(version-control t)
 '(visible-bell t)
 '(visible-mark-faces (quote (visible-mark-face1 visible-mark-face2)))
 '(visible-mark-max 2)
 '(wgrep-auto-save-buffer t)
 '(winner-mode t nil (winner) "Use C-c <left|right> to go back to previous windows configuration")
 '(zoom-frame/buffer (quote frame)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-mode-line ((t (:foreground "black" :weight bold))))
 '(aw-leading-char-face ((t (:height 6.0 :underline nil :foreground "red"))))
 '(column-marker-1 ((t (:underline t))))
 '(column-marker-2 ((t (:underline t))))
 '(column-marker-3 ((t (:underline t))))
 '(hl-line ((t (:underline t))))
 '(pillar-description-data-face ((t (:foreground "gainsboro" :slant italic)))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(package-initialize)

;;; use-package initialization
(eval-when-compile
  (require 'use-package)
  (setq use-package-verbose (null byte-compile-current-file)))

(defun set-selected-frame-dark ()
  (interactive)
  (let ((frame-name (cdr (assq 'name (frame-parameters (selected-frame))))))
    (call-process-shell-command
     (format
      "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT 'dark' -name '%s'"
      frame-name))))

(defun my:setup-frame ()
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (when (window-system)
    (ignore-errors (load-theme 'zerodark))
    (set-selected-frame-dark)
    (set-face-attribute 'default nil :height 115 :family "Fira Mono")))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (my:setup-frame)))
  (my:setup-frame))

(defun suspend-on-tty-only ()
  (interactive)
  (unless window-system
    (suspend-frame)))

(bind-key "C-x C-z" 'suspend-on-tty-only)

(add-to-list 'default-frame-alist '(cursor-type bar . 3))

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

(bind-key "<f5>" 'comment-region)

;; Replace `just-one-space' by the more advanced `cycle-spacing'.
(bind-key "M-SPC" #'cycle-spacing)

(defun my-join-line ()
  (interactive)
  (join-line -1))

(bind-key "M-j" 'my-join-line)

(defun toggle-window-split ()
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

(define-key ctl-x-4-map "t" 'toggle-window-split)

;; faster than C-x z
(bind-key "C-z" 'repeat)
(unbind-key "C-x z")

(define-prefix-command 'endless/toggle-map)
(setq endless/toggle-prefix "C-. t")
(bind-key endless/toggle-prefix 'endless/toggle-map)

(bind-key "c" 'flycheck-mode endless/toggle-map)
(bind-key "d" 'toggle-debug-on-error endless/toggle-map)
(bind-key "f" 'auto-fill-mode endless/toggle-map)
(bind-key "l" 'toggle-truncate-lines endless/toggle-map)
(bind-key "q" 'toggle-debug-on-quit endless/toggle-map)
(bind-key "r" 'dired-toggle-read-only endless/toggle-map)

(bind-key "C-x 8 <S-right>" (lambda () (interactive) (insert-char ?→))) ; rightwards arrow
(bind-key "C-x 8 <right>" (lambda () (interactive) (insert-char ?⇒))) ; rightwards double arrow

(bind-key "C-x 8 <S-left>" (lambda () (interactive) (insert-char ?←))) ; leftwards arrow
(bind-key "C-x 8 <left>" (lambda () (interactive) (insert-char ?⇐))) ; leftwards double arrow

(bind-key "C-x 8 <S-up>" (lambda () (interactive) (insert-char ?↑))) ; upwards arrow
(bind-key "C-x 8 <up>" (lambda () (interactive) (insert-char ?⇑))) ; upwards double arrow

(bind-key "C-x 8 <S-down>" (lambda () (interactive) (insert-char ?↓))) ; downwards arrow
(bind-key "C-x 8 <down>" (lambda () (interactive) (insert-char ?⇓))) ; rightwards double arrow

(bind-key "<S-left>" #'beginning-of-buffer)
(bind-key "<S-right>" #'end-of-buffer)
(unbind-key "M-<")
(unbind-key "M->")

(use-package ethan-wspace
  :diminish ethan-wspace-mode
  :demand t
  :config
  (progn
    (setq mode-require-final-newline nil) ;; superseded
    (global-ethan-wspace-mode)))

(use-package dired
  :defer t
  :bind (("C-x C-j" . dired-jump))
  :config
  (progn
    (use-package runner)
    (use-package dired-x)
    (use-package dired-imenu :demand t)

    (add-hook 'dired-mode-hook #'turn-on-gnus-dired-mode)

    (bind-key ")" 'dired-omit-mode dired-mode-map)

    (let ((extensions-to-ignore '(".out" ".lol" ".ali" ".upload" ".build" ".dsc" ".synctex.gz")))
      (mapc (lambda (extension)
              (add-to-list 'completion-ignored-extensions extension)
              (add-to-list 'dired-omit-extensions extension))
            extensions-to-ignore))

    (let ((files-to-ignore '("Thumbs\.db" "Thumbs\.db:encryptable" "b~.*\.ad[sb]")))
      (mapc (lambda (filename)
              (setq dired-omit-files
                    (concat dired-omit-files "\\|^" filename "$")))
            files-to-ignore))

    (add-hook 'dired-mode-hook (lambda () (dired-omit-mode) (dired-hide-details-mode 1)))

    (defun dired-move-beginning-of-line ()
      (interactive)
      (let ((point (point)))
        (dired-move-to-filename)
        (when (= point (point))
          (move-beginning-of-line nil))))

    (define-key dired-mode-map
      (vector 'remap 'move-beginning-of-line) 'dired-move-beginning-of-line)))

(use-package recentf
  :defer t
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

(use-package image-dired
  :defer t
  :config
  (progn
    (setq image-dired-cmd-create-thumbnail-options
          (replace-regexp-in-string "-strip" "-auto-orient -strip" image-dired-cmd-create-thumbnail-options)
          image-dired-cmd-create-temp-image-options
          (replace-regexp-in-string "-strip" "-auto-orient -strip" image-dired-cmd-create-temp-image-options))))

(use-package eshell
  :defer t
  :config
  (progn
    (require 'magit)
    (defconst my:prompt-defaults
      `(("username" . (user-login-name))
        ("hostname" . (substring (shell-command-to-string "hostname") 0 -1))))

    (defconst my:prompt-default-values
      (mapcar (lambda (pair) (cons (car pair) (eval (cdr pair))))
              my:prompt-defaults))

    (defun my:prompt-value (variable)
      "Returns current value for VARIABLE if it's not default, "" otherwise."
      (let ((default (cdr (assoc variable my:prompt-default-values)))
            (current (eval (cdr (assoc variable my:prompt-defaults)))))
        (if (string= default current)
            ""
          current)))

    (defvar my:prompt-inserted nil)

    (defun my:color (type)
      (require 'cl-lib)
      (cl-case type
        ('alert "red")
        ('good "lime green")
        ('warning "dark orange")
        (t "black")))

    (defun my:mprint (obj &optional color)
      (if color
          (insert (propertize obj 'face `(:foreground ,(my:color color))))
        (insert obj)))

    (defun my:insert-value (value &optional color)
      (my:mprint value color)
      (setq my:prompt-inserted (not (string= value ""))))

    (defun my:insert-variable (variable)
      (my:insert-value (my:prompt-value variable)))

    (defun my:insert-separator (&optional separator)
      (let ((sep (or separator ":")))
        (if my:prompt-inserted
            (my:mprint sep))))

    (defun my:insert-pwd ()
      (let ((pwd (abbreviate-file-name (eshell/pwd))))
        (my:insert-value pwd)))

    (defun my:git-color ()
      "Returns a color code based on the current repository status"
      (if (zerop (magit-git-exit-code "diff" "--quiet"))
          ;; nothing to commit because nothing changed
          (if (zerop (length (magit-git-string
                              "rev-list" (concat "origin/"
                                                 (magit-get-current-branch)
                                                 ".."
                                                 (magit-get-current-branch)))))
              ;; nothing to push as well
              'good
            ;; nothing to commit, but some commits must be pushed
            'warning)
        'alert))

    (defun my:insert-branch ()
      (let ((branch (magit-get-current-branch)))
        (if branch
            (my:mprint (concat " <" branch ">") (my:git-color)))))

    (defun my:eshell-prompt ()
      (with-temp-buffer
        (my:insert-variable "username")
        (my:insert-separator "@")
        (my:insert-variable "hostname")
        (my:insert-separator ":")
        (my:insert-pwd)
        (my:insert-branch)
        (my:insert-value (if (= (user-uid) 0) " # " " $ "))
        (buffer-substring (point-min) (point-max))))

    (eval-after-load "em-prompt"
      '(progn
         (setq eshell-highlight-prompt nil)
         (setq eshell-prompt-function 'my:eshell-prompt)))

    (eval-after-load "em-term"
      '(progn
         (add-to-list 'eshell-visual-commands "htop")
         (add-to-list 'eshell-visual-commands "journalctl")
         (add-to-list 'eshell-visual-commands "npm")
         (add-to-list 'eshell-visual-commands "bower")))))

(use-package ediff
  :defer t
  :config
  (progn
    (setq-default ediff-auto-refine 'on)))

(use-package magit
  :diminish (magit-auto-revert-mode magit-wip-after-save-mode magit-wip-after-apply-mode magit-wip-affter-change)
  :bind (("C-x g" . magit-status) ("C-x G" . magit-dispatch-popup))
  :config
  (progn
    (when magit-wip-before-change-mode
      (add-to-list 'magit-no-confirm #'safe-with-wip))

    ;; prevents magit from showing ugly passphrase dialog
    (setenv "SSH_ASKPASS" nil)))

(use-package info
  :bind ("C-h i" . my:info-other-window)
  :config
  (progn
    ;; Redefines info-other-window to use features of `info'
    (defun my:info-other-window (&optional file-or-node buffer)
      "Like `info' but show the Info buffer in another window."
      (interactive
       (list
        (if (and current-prefix-arg
                 (not (numberp current-prefix-arg)))
            (read-file-name "Info file name: " nil nil t))
        (if (numberp current-prefix-arg)
            (format "*info*<%s>" current-prefix-arg))))
      (info-setup
       file-or-node
       (switch-to-buffer-other-window (or buffer "*info*"))))))

(use-package ace-link
  :init
  (progn
    (ace-link-setup-default)))

(use-package ispell
  :bind
  (("C-. d b" . ispell-buffer)
   ("C-. d f" . ispell-change-dictionary-to-french)
   ("C-. d e" . ispell-change-dictionary-to-english)
   ("C-. d ?" . ispell-change-dictionary))

  :init
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
      (flyspell-buffer))

    (defun flyspell-toggle ()
      (interactive)
      (let ((mode-value (if flyspell-mode -1 1)))
        (save-excursion
          (add-file-local-variable 'eval `(flyspell-mode ,mode-value)))
        (flyspell-mode mode-value)))))

(use-package flyspell
  :diminish flyspell-mode
  :bind (("C-. f b" . flyspell-buffer)
         ("C-. f m" . flyspell-toggle))
  :init
  (progn
    (add-hook 'text-mode-hook 'flyspell-mode))
  :config
  (progn
    (unbind-key "C-." flyspell-mode-map)))

(use-package eldoc
  :diminish eldoc-mode
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)))

(use-package checkdoc
  :defer t
  :diminish checkdoc-minor-mode)

(use-package face-remap
  :defer t
  :diminish text-scale-mode)

(use-package flycheck
  :defer t
  :diminish flycheck-mode
  :config
  (progn
    (use-package flycheck-cask
      :config
      (progn
        (add-hook 'flycheck-mode-hook #'flycheck-cask-setup)))))

(use-package org
  :defer t
  :bind
  (("C-. o t"   . org-capture)
   ("C-. o a"   . org-agenda)
   ("C-. o SPC" . dc/my-agenda)
   ("C-. o l"   . org-store-link)
   ("C-. o w"   . my:org-move-to-refile-target)
   ("C-. o s"   . org-save-all-org-buffers))
  :init
  (progn
    (setq org-modules '(org-protocol org-capture ox-beamer))

    (defun my:org-move-to-refile-target (&optional last)
      (interactive "p")
      (require 'org)
      (org-refile (if (= last 4) '(16) '(4))))

    (defun dc/my-agenda ()
      (interactive)
      (require 'org-agenda)
      (let ((entry (assoc " " org-agenda-custom-commands)))
        (org-agenda-run-series (nth 1 entry) (cddr entry)))))
  :config
  (progn
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
          '((sequence "TODO(t)"    "|" "DONE(d)" "CANCELLED(c)")
            (sequence "APPT(p)"    "|" "DONE(d)" "CANCELED(c)")
            (sequence "WAITING(w)" "|" "DONE(d)")))

    (setq org-capture-templates
          '(("t" "Todo" entry
             (file org-default-notes-file)
             "* TODO %?%i")
            ("s" "Schedule" entry
             (file org-default-calendar-file)
             "* %?\n%^T")))

    (unbind-key "C-'" org-mode-map)

    (add-to-list 'org-file-apps '("\\.png\\'" . default))

    (defvar fuzz-factor 1.0e-3)
    (defun approx-equal (x y)
      (or (= x y)
          (< (/ (abs (- x y))
                (max (abs x) (abs y)))
             fuzz-factor)))

    (defvar my:gisele-service 192)
    (defun my:calc-gisele (other tp)
      (let (new-other new-tp)
        (if (< other 192)
            (progn
              (setq new-tp (max 0 (- tp (- my:gisele-service other))))
              (setq new-other (min my:gisele-service (+ other tp))))
          (setq new-tp (/ tp 2.0))
          (setq new-other (+ other (/ tp 2.0))))
        (+ new-other (* new-tp (/ 2.0 3)))))

    ;; When service is complete without TP:
    ;; 15h TP counts for 50% at 1 and for 50% at 2/3
    ;; 196.5 + (15/2) + (15/2 * 2/3)
    (cl-assert (approx-equal (my:calc-gisele 196.5 15) 209))

    ;; When service requires TP to be complete:
    ;; 15h TP first completes 180.5 to attain 192. The rest
    ;; 15-(192-180.5) counts for 2/3
    (cl-assert (approx-equal (my:calc-gisele 180.5 15) 194.33))

    ;; When service + TP is still not complete: (untested with Gisele)
    (cl-assert (approx-equal (my:calc-gisele 100 15) 115))

    (use-package ox-twbs)))

(use-package calc
  :defer t
  :config
  (progn
    (defmath vwsum (vec1 vec2)
      "Weighted sum: VEC1 are marks and VEC2 are coefficients."
      (+ (* vec1 vec2)))

    (defun my:round-to-half (num)
      (let* ((decimal (- num (ftruncate num))))
        (cond
         ((or (zerop decimal) (= 0.5 decimal)) num)
         ((< decimal 0.5) (+ 0.5 (ftruncate num)))
         ((> decimal 0.5) (1+ (ftruncate num))))))))

(use-package winner
  :defer t
  :init
  (progn
    (defun winner:undo-again ()
      (interactive)
      (setq this-command 'winner-undo)
      (winner-undo))

    (defun winner:redo-again ()
      (interactive)
      (setq this-command 'winner-undo)
      (winner-redo))

    (defun winner:prepare-for-futher-undo ()
      "Let one undo more by just pressing the last key.
Using standard configuration and this function, the user will be
able to type <C-c left left left> to undo 3 times whereas it was
<C-c left C-c left C-c left> before."
      (set-temporary-overlay-map
       (let ((map (make-sparse-keymap)))
         (define-key map (kbd "<left>") 'winner:undo-again)
         (define-key map (kbd "<right>") 'winner:redo-again)
         map) t)
      (message "Type <left>/<right> to continue switching"))

    (defun winner:initial-undo ()
      (interactive)
      (setq this-command 'winner-undo)
      (winner-undo)
      (winner:prepare-for-futher-undo))

    (bind-key "C-c <left>" 'winner:initial-undo  winner-mode-map)))

(use-package drag-stuff
  :diminish drag-stuff-mode
  :init
  (progn
    (drag-stuff-global-mode t)
    (add-to-list 'drag-stuff-except-modes 'org-mode)
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
  :diminish guide-key-mode
  :config
  (progn
    (setq guide-key/guide-key-sequence '("C-x 4" "C-c @" "C-. p" "C-. t" "C-x RET"))
    (add-to-list 'guide-key/guide-key-sequence endless/toggle-prefix)

    (with-eval-after-load "helm-config"
      (add-to-list 'guide-key/guide-key-sequence helm-command-prefix-key))

    (guide-key-mode 1)))

(use-package discover
  :init
  (progn
    (global-discover-mode 1)))

(use-package pillar
  :mode ("\\.\\(pier\\|pillar\\)\\'" . pillar-mode))

(use-package projectile
  :diminish projectile-mode
  :init
  (progn
    (projectile-global-mode)
    (use-package helm-projectile
      :init
      (progn
        (helm-projectile-on)))))

(use-package unify-opening
  :load-path "packages/unify-opening"
  :demand t)

(eval-and-compile
  (setq-default notmuch-command (executable-find "notmuch")))

(use-package notmuch
  :if notmuch-command
  :load-path (lambda () (and notmuch-command
                        (list (expand-file-name
                               "../../share/emacs/site-lisp"
                               (file-symlink-p notmuch-command)))))
  :bind (("C-. m" . notmuch))
  :config
  (progn
    (setq notmuch-message-headers '("To" "Cc" "Subject" "Date"))

    (defun my:mm-ics-to-org-part (handle &optional prompt)
      "Add message part HANDLE to org."
      (mm-with-unibyte-buffer
        (mm-insert-part handle)
        (mm-add-meta-html-tag handle)
        (require 'org-caldav)
        (my:import-ics-buffer-to-org)))

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
      (bind-key "R" #'notmuch-show-reply-sender notmuch-show-mode-map))

    (use-package profile
      :demand t
      :init
      (progn
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

        (setq notmuch-archive-tags '("-inbox" "-unread"))

        (setq notmuch-saved-searches
              `((:name "inbox" :query ,(profile-inbox-query) :key "i")
                (:name "noisy" :query ,(profile-noisy-unarchived-list-query) :key "n")
                (:name "ftgp" :query "damien.cassou@foretagsplatsen.se AND tag:inbox" :key "f")
                (:name "unread" :query "tag:unread" :key "u")
                (:name "sent" :query ,(profile-sent-query) :key "s"))))
      :config
      (progn
        (with-eval-after-load "notmuch-hello"
          (add-to-list 'notmuch-hello-sections
                       #'profile-queue-insert-section
                       t))))))

(use-package profile
  :demand t
  :load-path "packages/profile"
  :init
  (progn
    (with-eval-after-load "message"
      (bind-key "C-c F" #'profile-force-profile-in-compose message-mode-map)))
  :config
  (progn
    (setq profile-binding-alist
          '(("GMail"
             (profile-maildir . "/GMail")
             (user-mail-address . "damien.cassou@gmail.com")
             (smtpmail-queue-dir . "~/Mail/GMail/queued-mail/")
             (smtpmail-local-domain . nil)
             (smtpmail-smtp-user . "damien.cassou@gmail.com")
             (smtpmail-smtp-server . "QUIT_GMAIL")
             (smtpmail-stream-type . starttls)
             (smtpmail-smtp-service . 587))
            ("Pro"
             (profile-maildir . "/Pro")
             (notmuch-fcc-dirs . "Pro/Sent")
             (user-mail-address . "damien.cassou@inria.fr")
             (smtpmail-queue-dir . "~/Mail/Pro/queued-mail/")
             (smtpmail-local-domain . nil)
             (smtpmail-smtp-user . "cassou")
             (smtpmail-smtp-server . "smtp.inria.fr")
             (smtpmail-stream-type . starttls)
             (smtpmail-smtp-service . 587))
            ("Lille1"
             (profile-maildir . "/Lille1")
             (notmuch-fcc-dirs . "Lille1/Sent")
             (user-mail-address . "damien.cassou@univ-lille1.fr")
             (smtpmail-queue-dir . "~/Mail/Lille1/queued-mail/")
             (smtpmail-local-domain . nil)
             (smtpmail-smtp-user . "cassou")
             (smtpmail-smtp-server . "smtps.univ-lille1.fr")
             (smtpmail-stream-type . starttls)
             (smtpmail-smtp-service . 587))
            ("Perso"
             (profile-maildir . "/Perso")
             (notmuch-fcc-dirs . "Perso/Sent")
             (user-mail-address . "damien@cassou.me")
             (smtpmail-queue-dir . "~/Mail/Perso/queued-mail/")
             (smtpmail-local-domain . "inria.fr")
             (smtpmail-smtp-user . "dcassou")
             (smtpmail-smtp-server . "bender.ldn-fai.net")
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
          "list:\"pharo-dev\" OR list:\"pharo-users\" OR list:\"smallwiki\" OR to:\"notmuch@notmuchmail.org\" OR to:\"offlineimap-project@lists.alioth.debian.org\" OR list:\"nix-dev\" OR \"cristal.univ-lille1.fr\" OR \"tous-lille.inria.fr\" OR \"online.inria.fr\" OR \"tous-les-personnels.univ-lille1.fr\" OR \"cristal-cr-cdl.univ-lille1.fr\" OR \"cristal-permanents.univ-lille1.fr\"")))

(use-package mml
  :defer t
  :init
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
  :defer t
  :config
  (progn
    ;;; The following make sure to use the right profile when sending
    ;;; the message (i.e., when pressing C-c C-c in message-mode).
    ;;; It's better to set the profile just before sending to be sure
    ;;; to use the profile related to the From: message field).
    (defun my:message-send-and-exit (&optional arg)
      "Set profile according to From field.
Designed to be called before `message-send-and-exit'."
      (profile-set-profile-from-message-from-field))
    (advice-add #'message-send-and-exit
                :before
                #'my:message-send-and-exit)))

(use-package epg-config
  :init
  (progn
    (setq epg-gpg-program (executable-find "gpg2"))))

(use-package image
  :config
  (imagemagick-register-types))

(use-package diminish
  :config
  (progn
    (eval-after-load "simple"
      `(progn
         (diminish 'overwrite-mode)))))

;; Fix for issue
;; https://bugs.launchpad.net/emacs-snapshot/+bug/1251176
(use-package iso-transl)

(use-package lisp-mode
  :defer t
  :bind (("C-c RET" . pp-macroexpand-last-sexp))
  :config
  (progn
    (with-eval-after-load "yasnippet"
      (add-hook 'emacs-lisp-mode-hook 'yas-minor-mode))

    (use-package paredit
      :diminish paredit-mode
      :config
      (progn
        (use-package eldoc)
        (eldoc-add-command 'paredit-backward-delete 'paredit-close-round)))

    (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook #'enable-paredit-mode)))

(use-package autorevert
  :defer t
  :diminish auto-revert-mode)

(use-package git-auto-commit-mode
  :diminish git-auto-commit-mode)

(use-package pos-tip
  :disabled)

(use-package skeletor
  :commands (skeletor-create-project))

(use-package smartscan
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook 'smartscan-mode)
    (add-hook 'pillar-mode-hook 'smartscan-mode)))

(use-package zoom-frm
  :bind (("C-x C-+" . zoom-in/out)
         ("C-x C--" . zoom-in/out)
         ("C-x C-=" . zoom-in/out)
         ("C-x C-0" . zoom-in/out)
         ("<C-mouse-4>" . zoom-in)
         ("<C-mouse-5>" . zoom-out)))

(use-package visible-mark
  :config
  (progn
    (global-visible-mark-mode 1)
    (defface visible-mark-active
      '((((type tty) (class mono)))
        (t (:background "magenta"))) "")))

(use-package paren-face
  :init
  (progn
    (global-paren-face-mode)))

(use-package find-func
  :config
  (progn
    (define-key 'help-command (kbd "C-l") 'find-library)
    (define-key 'help-command (kbd "C-f") 'find-function)
    (define-key 'help-command (kbd "C-k") 'find-function-on-key)
    (define-key 'help-command (kbd "C-v") 'find-variable)))

(use-package git-timemachine)

(use-package anzu
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :diminish anzu-mode
  :config
  (progn
    (global-anzu-mode +1)))

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
    (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
    (add-hook 'css-mode-hook #'aggressive-indent-mode)))

(use-package helm
  :diminish helm-mode
  :bind (("M-x"     . helm-M-x)
         ("M-y"     . helm-show-kill-ring)
         ("C-x b"   . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x d"   . helm-find-files)
         ("M-i"     . helm-semantic-or-imenu)
         ("C-h SPC" . helm-all-mark-rings)
         ("C-:"     . helm-eval-expression-with-eldoc))
  :init
  (progn
    (require 'helm-config))
  :config
  (progn
    (bind-key "<S-left>" #'helm-beginning-of-buffer helm-map)
    (bind-key "<S-right>" #'helm-end-of-buffer helm-map)


    (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-i")   #'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-z")   #'helm-select-action)

    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
          helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
          helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
          helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
          helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
          helm-ff-file-name-history-use-recentf t)

    (with-eval-after-load "eshell"
      (require 'helm-eshell)

      (add-hook 'eshell-mode-hook
                #'(lambda ()
                    (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history))))

    (define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
    (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

    (with-eval-after-load "projectile"
      (require 'helm-projectile))

    (require 'helm-descbinds)

    (add-hook 'helm-grep-mode-hook #'(lambda () (use-package "wgrep")))

    (helm-descbinds-mode)
    (helm-mode 1)))

(use-package youtube
  :load-path "packages")

(use-package wgrep
  :init
  (progn
    (with-eval-after-load "helm-grep"
      (bind-key  "C-x C-q" #'wgrep-change-to-wgrep-mode helm-grep-mode-map)
      (bind-key "C-c C-c" #'wgrep-finish-edit helm-grep-mode-map))
    (with-eval-after-load "helm-ag"
      (bind-key  "C-x C-q" #'wgrep-change-to-wgrep-mode helm-ag-mode-map)
      (bind-key "C-c C-c" #'wgrep-finish-edit helm-ag-mode-map))))

(use-package password-store
  :config
  (progn
    (setq password-store-password-length 16)))

(use-package ace-window
  :demand t
  :bind (("M-o" . ace-window))
  :init
  (progn
    ;; force this binding even if another mode tries to use it
    (bind-key* "M-o" #'ace-window))
  :config
  (progn
    (ace-window-display-mode)
    ;; Use same keys for both ace-window and avy
    (setq aw-keys avy-keys)))

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
          org-caldav-sync-changes-to-org 'all)

    (defun my:import-ics-buffer-to-org ()
      "Add ics content in current buffer to `org-caldav-inbox'."
      (let ((event (org-caldav-convert-event))
            (file (org-caldav-inbox-file org-caldav-inbox)))
        (with-current-buffer (find-file-noselect file)
          (let* ((point-and-level (org-caldav-inbox-point-and-level org-caldav-inbox))
                 (point (car point-and-level))
                 (level (cdr point-and-level)))
            (goto-char point)
            (apply #'org-caldav-insert-org-entry
                   (append event (list nil level)))
            (message "%s: Added event: %s"
                     file
                     (buffer-substring
                      point
                      (save-excursion
                        (goto-char point)
                        (point-at-eol 2))))))))

    (defun my:import-ics-to-org (path)
      (with-current-buffer (get-buffer-create "*import-ics-to-org*")
        (delete-region (point-min) (point-max))
        (insert-file-contents path)
        (my:import-ics-buffer-to-org)))))

(use-package avy
  :bind (("C-," . avy-goto-char-2)
         ("M-g g" . avy-goto-line))
  :init
  (progn
    ;; keys under my fingers (blue keys on my Kinesis Advantage)
    (setq avy-keys '(?a ?s  ?d  ?f  ?j  ?k  ?l  59 32))

    ;; forces this binding even if another mode tries to use it
    (bind-key* "C-," #'avy-goto-char-2)))

(use-package outline
  :diminish outline-minor-mode)

(use-package define-word
  :bind (("C-. D" . define-word-at-point)))

(use-package flycheck-package
  :init
  (with-eval-after-load "flycheck"
    (flycheck-package-setup)))

(use-package beginend
  :diminish (beginend-dired-mode beginend-message-mode)
  :load-path "packages/beginend"
  :config
  (progn
    (beginend-setup-all)))

(use-package avy-zap
  :disabled t
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

(use-package vdirel
  :load-path "packages/vdirel"
  :bind (("C-. c" . vdirel-helm-select-email)))

(use-package auth-password-store
  :demand t
  :load-path "packages/auth-password-store"
  :init
  (progn
    (setq auth-sources '(password-store))))

(use-package help
  :defer t
  :config
  (progn
    (defun my:revert-buffer-no-confirm (&optional ignore-auto)
      "Revert current buffer without asking."
      (interactive (list (not current-prefix-arg)))
      (revert-buffer ignore-auto t nil))
    ;; Make sure `g' reverts buffer without confirmation
    (bind-key (vector 'remap 'revert-buffer)
              #'my:revert-buffer-no-confirm
              help-mode-map)))

(use-package nameless
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook #'nameless-mode)))

(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

(require 'beacon)
;; https://github.com/Malabarba/beacon/pull/50
(defun beacon--color-range ()
  "Return a list of background colors for the beacon."
  (let* ((default-bg (or (save-excursion
                           (unless (eobp)
                             (forward-line 1)
                             (unless (or (bobp) (not (bolp)))
                               (forward-char -1)))
                           (background-color-at-point))
                         (face-background 'default)))
         (bg (color-values (if (or (not (stringp default-bg))
                                   (string-match "\\`unspecified-" default-bg))
                               (face-attribute 'beacon-fallback-background :background)
                             default-bg)))
         (fg (cond
              ((stringp beacon-color) (color-values beacon-color))
              ((and (stringp bg)
                    (< (color-distance "black" bg)
                       (color-distance "white" bg)))
               (make-list 3 (* beacon-color 65535)))
              (t (make-list 3 (* (- 1 beacon-color) 65535))))))
    (when bg
      (apply #'seq-mapn (lambda (r g b) (format "#%04x%04x%04x" r g b))
             (mapcar (lambda (n) (butlast (beacon--int-range (elt fg n) (elt bg n))))
                     [0 1 2])))))

(use-package beacon
  :init
  (progn
    (beacon-mode)))

(use-package widgetjs
  :load-path "packages/emacs-js/widgetjs")

(use-package emacs-js
  :load-path "packages/emacs-js"
  :config
  (progn
    (use-package monitor-setup)
    (setenv "PATH" (concat (getenv "PATH") ":/home/cassou/node_modules/.bin"))
    (add-to-list 'exec-path "/home/cassou/node_modules/.bin")))

;;; Emacs Configuration
;; Local Variables:
;; eval: (outline-minor-mode)
;; End:
