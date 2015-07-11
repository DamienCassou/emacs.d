;;; -*- Mode: Emacs-Lisp -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-section-hook
   (quote
    (LaTeX-section-heading LaTeX-section-title LaTeX-section-section)))
 '(LaTeX-verbatim-environments
   (quote
    ("verbatim" "verbatim*" "lstlisting" "comment" "Verbatim")))
 '(TeX-PDF-mode t)
 '(TeX-auto-save t)
 '(TeX-default-unit-for-image "\\linewidth")
 '(TeX-master nil)
 '(TeX-parse-self t)
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(auth-source-do-cache nil nil nil "Don't save as I'm doing experiments with gnupg")
 '(avy-style (quote at-full))
 '(aw-scope (quote frame))
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(bibtex-align-at-equal-sign t)
 '(bibtex-autokey-name-case-convert-function (quote identity))
 '(bibtex-autokey-name-length 4)
 '(bibtex-autokey-titlewords 0)
 '(bibtex-entry-format
   (quote
    (opts-or-alts required-fields whitespace realign last-comma delimiters)))
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
 '(flyspell-tex-command-regexp
   "\\(\\(begin\\|end\\)[ 	]*{\\|\\(cite[a-z*]*\\|label\\|ct\\|c?cauthor\\|sigle\\|\\(lst\\)?\\(lignesa\\|lignes\\|ligne\\)\\|nocheck\\|macitation\\|enword\\|ref\\|eqref\\|pageref\\|page\\|listing\\|usepackage\\|documentclass\\)[ 	]*\\(\\[[^]]*\\]\\)?{[^{}]*\\)")
 '(flyspell-use-meta-tab nil)
 '(font-latex-match-bold-command-keywords
   (quote
    (("mn" "{")
     ("damien" "{")
     ("dc" "{")
     ("eb" "{")
     ("bb" "{")
     ("cc" "{")
     ("jl" "{")
     ("sd" "{")
     ("dp" "{")
     ("nl" "{")
     ("cam" "{"))))
 '(font-latex-match-math-command-keywords (quote (("mm" "{") ("contract" "{{{"))))
 '(font-latex-match-reference-keywords (quote (("ccauthor" "[{") ("cauthor" "{"))))
 '(font-latex-match-type-command-keywords
   (quote
    (("ct" "{")
     ("method" "{")
     ("class" "{")
     ("lct" "{"))))
 '(frame-title-format "Emacs: %b" t)
 '(gc-cons-threshold 20000000)
 '(global-font-lock-mode t)
 '(global-hl-line-mode t)
 '(global-hl-line-sticky-flag t)
 '(global-pair-mode t)
 '(global-prettify-symbols-mode t)
 '(haskell-hoogle-command "hoogle")
 '(imenu-auto-rescan t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(initial-major-mode (quote text-mode))
 '(initial-scratch-message nil)
 '(io-command
   "/home/cassou/Downloads/stevedekorte-io-f641230/build/_build/binaries/io")
 '(kept-new-versions 6)
 '(kept-old-versions 10)
 '(load-prefer-newer t)
 '(magit-diff-refine-hunk t)
 '(magit-repository-directories
   (quote
    ("~/Documents/writing" "~/Documents/candidatures" "~/Documents" "~/.emacs.d/packages" "~/.emacs.d/themes" "~/Documents/smalltalk" "~/tmp/emacs-configurations" "~/tmp" "~/Documents/projects" "~/Documents/websites" "~/Documents/teaching" "~/")))
 '(magit-repository-directories-depth 1)
 '(magit-revert-buffers t)
 '(magit-wip-after-apply-mode t)
 '(magit-wip-after-save-mode t)
 '(magit-wip-before-change-mode t)
 '(menu-bar-mode nil)
 '(message-default-charset (quote utf-8))
 '(message-log-max t)
 '(message-send-mail-function (quote message-smtpmail-send-it))
 '(message-signature t)
 '(message-signature-file "~/.signature")
 '(mu4e-attachment-dir "/tmp")
 '(mu4e-completing-read-function (quote completing-read))
 '(mu4e-compose-complete-addresses nil nil nil "Use pycarddavel instead")
 '(mu4e-compose-signature t)
 '(mu4e-drafts-folder "/Drafts")
 '(mu4e-get-mail-command "true" nil nil "Disable fetching email as it is done by a daemon")
 '(mu4e-headers-date-format "%Y/%m/%d %H:%M")
 '(mu4e-headers-fields
   (quote
    ((:human-date . 16)
     (:from-or-to . 20)
     (:mailing-list . 16)
     (:tags . 10)
     (:maildir . 15)
     (:subject))))
 '(mu4e-headers-time-format "%H:%M")
 '(mu4e-html2text-command (quote mu4e-shr2text))
 '(mu4e-maildir "~/Mail")
 '(mu4e-update-interval nil)
 '(mu4e-use-fancy-chars t)
 '(mu4e-user-mail-address-list
   (quote
    ("damien@cassou.me" "damien.cassou@gmail.com" "damien.cassou@lifl.fr" "damien.cassou@inria.fr" "cassou@inria.fr" "damien.cassou@laposte.net" "damien.cassou@univ-lille1.fr")))
 '(mu4e-view-fields
   (quote
    (:from :to :cc :subject :date :mailing-list :tags :flags :attachments :signature :decryption :maildir)))
 '(mu4e-view-show-addresses t)
 '(mu4e-view-show-images t)
 '(next-screen-context-lines 5)
 '(notmuch-labeler-hide-known-labels t)
 '(org-babel-load-languages (quote ((sh . t) (emacs-lisp . t) (java . t) (python . t))))
 '(org-catch-invisible-edits (quote error))
 '(org-clock-clocked-in-display nil)
 '(org-completion-use-ido t)
 '(org-default-notes-file "refile.org")
 '(org-directory "~/Documents/configuration/org")
 '(org-export-allow-bind-keywords t)
 '(org-export-with-toc nil)
 '(org-fontify-done-headline t)
 '(org-hide-leading-stars t)
 '(org-html-postamble nil)
 '(org-imenu-depth 2)
 '(org-latex-tables-booktabs t)
 '(org-log-done (quote time))
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-use-outline-path (quote full-file-path))
 '(org-special-ctrl-a/e t)
 '(org-startup-align-all-tables t)
 '(org-time-stamp-rounding-minutes (quote (10 10)))
 '(org-use-speed-commands t)
 '(powerline-display-buffer-size nil)
 '(proced-filter (quote all))
 '(projectile-completion-system (quote helm))
 '(projectile-keymap-prefix (kbd "C-. p"))
 '(projectile-require-project-root nil)
 '(read-file-name-completion-ignore-case t)
 '(recentf-auto-cleanup 300)
 '(recentf-exclude (quote ("~$" "\\.log$")))
 '(recentf-max-saved-items 4000)
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/recentf")
 '(reftex-default-bibliography
   (quote
    ("~/Documents/rmodbib/bib/rmod.bib" "~/Documents/rmodbib/bib/others.bib")))
 '(reftex-plug-into-AUCTeX t)
 '(reftex-view-crossref-cite-macros "\\`\\\\cite\\|cite\\*?\\'\\|bibentry\\|ccauthor")
 '(reftex-view-crossref-extra nil)
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
 '(smex-save-file "~/.emacs.d/smex-items")
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
 '(user-full-name "Damien Cassou")
 '(user-mail-address "damien.cassou@gmail.com")
 '(vc-follow-symlinks nil)
 '(vc-make-backup-files t)
 '(version-control t)
 '(visible-bell t)
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
 '(hl-line ((t (:underline t))))
 '(mode-line ((t (:background "#bd6626" :foreground "#f6f3e8"))))
 '(mode-line-highlight ((t (:foreground unspecified :background unspecified :weight bold))))
 '(mode-line-inactive ((t (:background "#454545" :foreground "#666666"))))
 '(mu4e-header-highlight-face ((t (:underline t))))
 '(org-agenda-done ((t (:foreground "LightSalmon" :strike-through t))))
 '(org-done ((t (:foreground "LightSalmon" :strike-through t :weight bold))))
 '(org-headline-done ((t (:foreground "LightSalmon" :strike-through t))))
 '(pillar-description-data-face ((t (:foreground "gainsboro" :slant italic))))
 '(powerline-active1 ((t (:inherit mode-line :background "#666666" :foreground "#f6f3e8"))))
 '(powerline-active2 ((t (:inherit mode-line :background "#252525" :foreground "#f6f3e8"))))
 '(powerline-inactive2 ((t (:inherit mode-line :background "#666666" :foreground "#aaaaaa"))))
 '(visible-mark-face1 ((t (:background "red" :foreground "black"))))
 '(visible-mark-face2 ((t (:background "salmon" :foreground "black")))))

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(defun darwinp ()
  (interactive)
  "Return true if system is darwin-based (Mac OS X)"
  (string-equal system-type "darwin"))

;; Path
(defun add-to-executable-path (path)
  (let ((expanded-path (expand-file-name path)))
    (add-to-list 'exec-path expanded-path)
    (setenv "PATH" (concat expanded-path ":" (getenv "PATH")))))

(mapc
 'add-to-executable-path
 (if (darwinp)
     (list  "/usr/local/bin"
            "/usr/local/sbin"
            "~/usr/apps/texlive/latest/bin/universal-darwin/")
   (list "~/Documents/configuration/scripts/"
         "/nix/store/ij052kind6xb6gnw4f0akc98ssqhq8dc-unzip-6.0/bin"
         "/home/cassou/.nix-profile/bin"
         "/home/cassou/.nix-profile/sbin"
         "/home/cassou/.nix-profile/lib/kde4/libexec"
         "/nix/var/nix/profiles/default/bin"
         "/nix/var/nix/profiles/default/sbin"
         "/nix/var/nix/profiles/default/lib/kde4/libexec"
         "/run/current-system/sw/bin"
         "/run/current-system/sw/sbin"
         "/run/current-system/sw/lib/kde4/libexec"
         "/var/setuid-wrappers"))) ;; the /var/setuid-wrappers/
                                   ;; directory must arrive first on
                                   ;; the PATH

(mapc 'add-to-executable-path '("~/.emacs.d/packages/cask/bin"))


(require 'cask "~/.emacs.d/packages/cask/cask.el")
(cask-initialize)

(require 'powerline nil t)
(with-eval-after-load 'powerline
  (powerline-default-theme))

(require 'bind-key "~/.emacs.d/packages/use-package/bind-key.el")
(require 'use-package "~/.emacs.d/packages/use-package/use-package.el")
(eval-when-compile
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
    (set-selected-frame-dark)))

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

; Replace `just-one-space' by the more advanced `cycle-spacing'.
(bind-key "M-SPC" #'cycle-spacing)

(defun my-join-line ()
  (interactive)
  (join-line -1))

(bind-key "M-j" 'my-join-line)

(defun toggle-window-split ()
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

(defvar buffers-to-keep '("*scratch*" "*Messages*"))
(defun buffer-killable-p (buffer)
  (and
   (not (member (buffer-name buffer) buffers-to-keep))
   (or (null (buffer-file-name buffer)) ;; buffer is not a file
       (not (buffer-modified-p buffer))))) ;; or file is not modified

(defun kill-all-buffers ()
  (interactive)
  (let ((count 0))
    (dolist (buffer (buffer-list))
      (when (buffer-killable-p buffer)
        (incf count)
        (message "Killing %s" (buffer-name buffer))
        (kill-buffer buffer)))
    (message "%s buffers have been killed" count)))

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

(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, subtree, or defun, whichever applies
first.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode) (org-narrow-to-subtree))
        (t (narrow-to-defun))))

(bind-key "n" 'narrow-or-widen-dwim endless/toggle-map)

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

    (bind-key ")" 'dired-omit-mode dired-mode-map)

    (when (darwinp)
      ;; Use coreutils from homebrew to provide a real ls
      (setq dired-use-ls-dired t)
      (setq insert-directory-program "gls"))

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

(use-package bibtex
  :defer t
  :config
  (progn
    (add-to-list 'bibtex-BibTeX-entry-alist
                      '("software" "A software"
                        (("title")
                         ("version")
                         ("organization")
                         ("month")
                         ("year")
                         ("id"))
                        nil
                        nil))

    ;; "keywords" should be a recognized field
    (mapc
     (lambda (list)
       (push '("keywords") (cl-fifth list)))
     bibtex-BibTeX-entry-alist)))

(use-package eshell
  :defer t
  :requires magit
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
         (add-to-list 'eshell-visual-commands "htop")))))

(use-package ediff
  :defer t
  :config
  (progn
    (setq-default ediff-auto-refine 'on)))

(use-package reftex
  :defer t
  :diminish reftex-mode
  :init
  (progn
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

  :config
  (progn
    (eval-after-load "reftex-vars"
      '(progn
         (add-to-list 'reftex-bibliography-commands
                      "bibliographyphd")
         (add-to-list 'reftex-bibliography-commands
                      "bibliographysoft")))))

(use-package magit
  :diminish (magit-auto-revert-mode magit-wip-after-save-mode magit-wip-after-apply-mode magit-wip-affter-change)
  :bind (("C-x g" . magit-status) ("C-x G" . magit-dispatch-popup))
  :config
  (progn
    (use-package magit-svn
      :defer t
      :init
      (progn
        (add-hook 'magit-mode-hook #'magit-svn-mode))
      :diminish magit-svn-mode)

    (when magit-wip-before-change-mode
      (add-to-list 'magit-no-confirm #'safe-with-wip))

    (defun magit-ignore-latex-project ()
      (interactive)
      (mapc
       #'magit-ignore-file
       (list "*.aux" "*.log" "*.out" "*.bbl" "*.blg" "auto/" "*.synctex.gz" "*.toc"))
      (magit-refresh))

    ;; http://endlessparentheses.com/easily-create-github-prs-from-magit.html
    ;; http://endlessparentheses.com/create-github-prs-from-emacs-with-magit.html
    (defun endless/visit-pull-request-url ()
      "Visit the current branch's PR on Github."
      (interactive)
      (browse-url
       (format "https://github.com/%s/pull/new/%s"
               (replace-regexp-in-string
                "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
                (magit-get "remote"
                           (magit-get-remote)
                           "url"))
               (cdr (magit-get-remote-branch)))))

    (define-key magit-mode-map "v"
      #'endless/visit-pull-request-url)

    ;; http://endlessparentheses.com/automatically-configure-magit-to-access-github-prs.html
    (defun endless/add-PR-fetch ()
      "If refs/pull is not defined on a GH repo, define it."
      (let ((fetch-address
             "+refs/pull/*/head:refs/pull/origin/*")
            (magit-remotes
             (magit-get-all "remote" "origin" "fetch")))
        (unless (or (not magit-remotes)
                    (member fetch-address magit-remotes))
          (when (string-match
                 "github" (magit-get "remote" "origin" "url"))
            (magit-git-string
             "config" "--add" "remote.origin.fetch"
             fetch-address)))))

    (add-hook 'magit-mode-hook #'endless/add-PR-fetch)))

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

(use-package ace-jump-mode
  :disabled t
  :bind ("C-," . ace-jump-mode)
  :init
  (progn
    (bind-key* "C-," 'ace-jump-mode)))

(use-package ace-link
  :defer t
  :init
  (progn
    (eval-after-load "info"
      `(progn
         (define-key Info-mode-map "o" 'ace-link-info)))
    (eval-after-load "help-mode"
      `(progn
         (define-key help-mode-map "o" 'ace-link-help)))))

(use-package ispell
  :defer t
  :requires flyspell
  :bind
  (("C-. d b"   . ispell-buffer)
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
        (flyspell-mode mode-value)))
    )
  :config
  ;; ispell must ignore LaTeX commands and environments
  (setq ispell-tex-skip-alists
        (list
         (append (car ispell-tex-skip-alists)
                 '(("\\\\cite"            ispell-tex-arg-end)
                   ("\\\\nocite"          ispell-tex-arg-end)
                   ("\\\\includegraphics" ispell-tex-arg-end)
                   ("\\\\figScale"         ispell-tex-arg-end)
                   ("\\\\author"          ispell-tex-arg-end)
                   ("\\\\ref"             ispell-tex-arg-end)
                   ("\\\\eqref"             ispell-tex-arg-end)
                   ("\\\\pageref"             ispell-tex-arg-end)
                   ("\\\\label"           ispell-tex-arg-end)
                   ("\\\\lstinputlisting" ispell-tex-arg-end)
                   ("\\\\enword" ispell-tex-arg-end)
                   ("\\\\ct" ispell-tex-arg-end)
                   ("\\\\sigle" ispell-tex-arg-end)
                   ("\\\\nocheck" ispell-tex-arg-end)
                   ("\\\\mathit" ispell-tex-arg-end)
                   ("\\\\url" ispell-tex-arg-end)
                   ("\\\\lst[p]?\\(lignesa\\|lignes\\|ligne\\)" ispell-tex-arg-end 2)
                   ("\\\\\\(lignesa\\|lignes\\|ligne\\)" ispell-tex-arg-end)
                   ("\\\\c?cauthor" ispell-tex-arg-end)
                   ("\\\\page" ispell-tex-arg-end)
                   ("\\\\listing" ispell-tex-arg-end)
                   ("\\\\macitationraw" ispell-tex-arg-end 2)
                   ("\\\\macitation" ispell-tex-arg-end 3)
                   ("\\\\mm" ispell-tex-arg-end)
                   ("\\\\begin{lstlisting}" . "\\\\end{lstlisting}")
                   ("\\\\begin{code}{}" . "\\\\end{code}")
                   ))
         (append (cadr ispell-tex-skip-alists)
                 '(("tabular" ispell-tex-arg-end)
                   ("equation\\*" . "\\\\end[ 	\n]*{[ 	\n]*equation\\*[ 	\n]*}")
                   ("tikzpicture" . "\\\\end[ 	\n]*{[ 	\n]*tikzpicture[ 	\n]*}"))))))

(use-package flyspell
  :defer t
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
    (setq org-modules '(org-publish org-protocol org-capture ox-beamer))

    (eval-after-load "org-latex"
      `(progn
         (setq org-latex-listings t)
         (add-to-list 'org-latex-packages-alist '("" "listings"))
         (add-to-list 'org-latex-packages-alist '("" "color"))))

    (defun my:org-move-to-refile-target (&optional last)
      (interactive "p")
      (org-refile (if (= last 4) '(16) '(4))))

    (defun dc/my-agenda ()
      (interactive)
      (require 'org-agenda)
      (let ((entry (assoc " " org-agenda-custom-commands)))
        (org-agenda-run-series (nth 1 entry) (cddr entry))))

    (defvar-local dc:org-publish-on-save nil
      "Set to t if you want to publish the project on each save.")
    (defun dc:org-publish-on-save ()
      "Publish the current project."
      (when dc:org-publish-on-save
        (save-excursion
          (org-publish-current-project))))
    (add-hook 'after-save-hook #'dc:org-publish-on-save))

  :config
  (progn
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

    (setq org-todo-keyword-faces
          '(("NEXT" :foreground "orange" :weight bold)
            ("CANCELLED" :foreground "forest green")))

    (setq org-capture-templates
          '(("t" "Todo" entry
             (file org-default-notes-file)
             "* TODO %?%i")
            ("u" "URL" entry
             (file org-default-notes-file)
             "* TODO %?%i\n %a")
            ("s" "Schefule" entry
             (file org-default-calendar-file)
             "* %?\n%^T")))

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

    (unbind-key "C-'" org-mode-map)

    (with-eval-after-load "yasnippet"
      (add-hook 'org-mode-hook 'yas-minor-mode))

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
    (cl-assert (approx-equal (my:calc-gisele 100 15) 115)))) 

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

(use-package auctex
  :mode ("\\.tex\\'" . latex-mode)
  :init
  (progn
    (require 'tex-mode)
    (load "auctex"))
  :config
  (progn
    (load "mybibtex" t t t)

    (add-to-list 'TeX-command-list
                 '("Bibtex all" "multibib/bibtexall" TeX-run-BibTeX
                   nil t :help "Run Bibtex on all aux files") t)

    (defadvice TeX-source-correlate-sync-source (after my:highlight-line-correlate activate)
      (when (require 'pulse nil t)
        (pulse-momentary-highlight-one-line (point))))

    (defun LaTeX-align-table ()
      (interactive)
      (save-excursion
        (LaTeX-mark-environment)
        (while (re-search-forward "& *" (region-end) t)
          (replace-match "& " nil nil))
        (LaTeX-mark-environment)
        (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)\\(&\\|\\\\\\\\\\)" 1 1 t)))))

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
  :defer t
  :bind (("C-M-'"   . shell-switcher-new-shell)
         ("C-'"     . shell-switcher-switch-buffer)
         ("C-x 4 '" . shell-switcher-switch-buffer-other-window)))

(use-package undo-tree
  :init
  (progn
    (global-undo-tree-mode)
    (define-key undo-tree-map (kbd "C-x r") nil)))

(use-package dired-toggle-sudo
  :defer t
  :bind (("C-x s" . dired-toggle-sudo)))

(use-package web-mode
  :defer t
  :mode ("\\.html?\\'" . web-mode)
  :init
  (progn
    (setq web-mode-engines-alist
          '(("liquid" .
             "jekyll/_layouts/.*\\.html\\'")))))

(use-package guide-key
  :diminish guide-key-mode
  :init
  (progn
    (setq guide-key/guide-key-sequence '("C-x 4" "C-c @" "C-. p" "C-. t"))

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

(use-package gnus-dired
  :disabled t
  :bind (("C-x C-a" . gnus-dired-attach))
  :defer t)

(use-package unify-opening
  :demand t)

(when (setq mu4e-mu-binary (executable-find "mu"))
  (add-to-list 'load-path (expand-file-name "../../share/emacs/site-lisp/mu4e" (file-symlink-p mu4e-mu-binary)))
  (use-package mu4e
    :bind (("C-. m" . mu4e))
    :config
    (progn
      (setq mu4e-action-tags-header "X-Keywords")
      (setq  mu4e-hide-index-messages nil)
      (imagemagick-register-types)

      (setenv "MU_PLAY_PROGRAM" "eopen")

      (add-to-list 'load-path "~/.emacs.d/packages/multimu4e")
      (use-package multimu4e
        :demand t
        :init
        (progn
          (add-hook 'mu4e-compose-pre-hook #'multimu4e-set-account-in-compose)
          (bind-key "C-c F" #'multimu4e-force-account-in-compose))
        :config
        (progn
          (setq multimu4e-account-alist
                '(("GMail"
                   (multimu4e-account-maildir . "/GMail")
                   (mu4e-trash-folder . "/GMail/Trash")
                   (mu4e-sent-folder . "/GMail/All Mail")
                   (mu4e-sent-messages-behavior . delete)
                   (mu4e-drafts-folder . "/GMail/Drafts")
                   (user-mail-address . "damien.cassou@gmail.com")
                   (smtpmail-queue-dir . "~/Mail/GMail/queued-mail/")
                   (smtpmail-local-domain . nil)
                   (smtpmail-smtp-user . "damien.cassou@gmail.com")
                   (smtpmail-smtp-server . "QUIT_GMAIL")
                   (smtpmail-stream-type . starttls)
                   (smtpmail-smtp-service . 587))
                  ("Pro"
                   (multimu4e-account-maildir . "/Pro")
                   (mu4e-trash-folder . "/Pro/Trash")
                   (mu4e-sent-folder . "/Pro/Sent")
                   (mu4e-sent-messages-behavior . sent)
                   (mu4e-drafts-folder . "/Pro/Drafts")
                   (user-mail-address . "damien.cassou@inria.fr")
                   (smtpmail-queue-dir . "~/Mail/Pro/queued-mail/")
                   (smtpmail-local-domain . nil)
                   (smtpmail-smtp-user . "cassou")
                   (smtpmail-smtp-server . "smtp.inria.fr")
                   (smtpmail-stream-type . starttls)
                   (smtpmail-smtp-service . 587))
                  ("Lille1"
                   (multimu4e-account-maildir . "/Lille1")
                   (mu4e-trash-folder . "/Lille1/Trash")
                   (mu4e-sent-folder . "/Lille1/Sent")
                   (mu4e-sent-messages-behavior . sent)
                   (mu4e-drafts-folder . "/Lille1/Drafts")
                   (user-mail-address . "damien.cassou@univ-lille1.fr")
                   (smtpmail-queue-dir . "~/Mail/Lille1/queued-mail/")
                   (smtpmail-local-domain . nil)
                   (smtpmail-smtp-user . "cassou")
                   (smtpmail-smtp-server . "smtps.univ-lille1.fr")
                   (smtpmail-stream-type . starttls)
                   (smtpmail-smtp-service . 587))
                  ("Perso"
                   (multimu4e-account-maildir . "/Perso")
                   (mu4e-trash-folder . "/Perso/Trash")
                   (mu4e-sent-folder . "/Perso/Sent")
                   (mu4e-sent-messages-behavior . sent)
                   (mu4e-drafts-folder . "/Perso/Drafts")
                   (user-mail-address . "damien@cassou.me")
                   (smtpmail-queue-dir . "~/Mail/Perso/queued-mail/")
                   (smtpmail-local-domain . "inria.fr")
                   (smtpmail-smtp-user . "dcassou")
                   (smtpmail-smtp-server . "bender.ldn-fai.net")
                   (smtpmail-stream-type . ssl)
                   (smtpmail-smtp-service . 465))))
          (multimu4e-set-account-from-name "Perso")))

      (defun my:mu4e-sent-query ()
        (mapconcat
         (lambda (address) (format "from:%s" address))
         mu4e-user-mail-address-list
         " OR "))

      (defun my:mu4e-noisy-mailinglist-query ()
        "list:pharo* OR list:smallwiki*")

      (defun my:mu4e-inbox-folder-or-tag-query ()
        (format "(%s OR maildir:\"/GMail/All Mail\") AND (tag:\\\\Inbox OR NOT maildir:\"/GMail/All Mail\")"
                (mapconcat
                 (lambda (account-name) (format "maildir:/%s/INBOX" account-name))
                 (multimu4e-account-names)
                 " OR ")))

      (defun my:mu4e-new-inbox-query ()
        (format "(%s) AND (NOT (%s) OR recip:damien*)"
                (my:mu4e-inbox-folder-or-tag-query)
                (my:mu4e-noisy-mailinglist-query)))

      (defun my:mu4e-noisy-unarchived-list-query ()
        (format "(%s) AND (%s)"
                (my:mu4e-inbox-folder-or-tag-query)
                (my:mu4e-noisy-mailinglist-query)))

      (setq mu4e-bookmarks
            `((,(my:mu4e-new-inbox-query)             "Inbox"          ?i)
              (,(my:mu4e-noisy-unarchived-list-query) "Pharo"          ?p)
              (,(my:mu4e-sent-query)                  "Sent"           ?s)
              ("tag:achats"                           "Achats"         ?a)
              ("size:20M..500M"                       "Large messages" ?l)))

      (require 'mu4e-contrib)
      (require 'smtpmail)
      (require 'gnus-dired)

      (require 'org-mu4e nil t)
      (with-eval-after-load 'org-mu4e
        (require 'cl)
        (lexical-let ((capture-letter "m"))
          (add-to-list
           'org-capture-templates
           `(,capture-letter "Mail" entry
                             (file org-default-notes-file)
                             "* TODO %?%:fromto %a"))))

      (defun my:mu4e-gmail-msg-p (msg)
        (require 's)
        (s-starts-with? "/GMail" (mu4e-message-field msg :maildir)))

      (defun my:mu4e-refile-folder (msg)
        (let ((maildir (mu4e-message-field msg :maildir)))
          (cond
           ;; messages to GMail
           ((my:mu4e-gmail-msg-p msg)
            (mu4e-action-retag-message msg "-\\Inbox")
            maildir)
           (t
            (require 'f)
            (f-expand "Archive" (f-parent maildir))))))

      (setq mu4e-refile-folder #'my:mu4e-refile-folder)

      (defun my:mu4e-trash-folder (msg)
        (let ((maildir (mu4e-message-field msg :maildir)))
          (cond
           ;; messages to GMail
           ((my:mu4e-gmail-msg-p msg)
            (mu4e-action-retag-message msg "+\\Trash")
            maildir)
           (t
            (require 'f)
            (f-expand "Trash" (f-parent maildir))))))

      (setq mu4e-trash-folder #'my:mu4e-trash-folder)

      (defun my:mu4e-remove-message-from-inbox (msg)
        (mu4e-action-retag-message msg "-\\Inbox"))

      (defun my:mu4e-remove-thread-from-inbox (docid &rest target)
        (mu4e~headers-goto-docid docid)
        (my:mu4e-remove-message-from-inbox (mu4e-message-at-point)))

      (add-to-list 'mu4e-marks
                   `(archive :char "a" :prompt "archive"
                             :show-target (lambda (target) "archive")
                             :action ,#'my:mu4e-remove-thread-from-inbox))

      (add-to-list 'mu4e-view-actions '("retag" . mu4e-action-retag-message))
      (add-to-list 'mu4e-headers-actions '("retag" . mu4e-action-retag-message))
      (add-to-list 'mu4e-view-actions '("archive" . my:mu4e-remove-message-from-inbox))
      (add-to-list 'mu4e-headers-actions '("archive" . my:mu4e-remove-message-from-inbox))
      (add-to-list 'mu4e-view-actions '("bview in browser" . mu4e-action-view-in-browser) t)

      ;; Attach files with dired
      ;; make the `gnus-dired-mail-buffers' function also work on
      ;; message-mode derived modes, such as mu4e-compose-mode
      (defun gnus-dired-mail-buffers ()
        "Return a list of active message buffers."
        (let (buffers)
          (save-current-buffer
            (dolist (buffer (buffer-list t))
              (set-buffer buffer)
              (when (and (derived-mode-p 'message-mode)
                         (null message-sent-message-via))
                (push (buffer-name buffer) buffers))))
          (nreverse buffers)))

      (setq gnus-dired-mail-mode 'mu4e-user-agent)
      (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

      (defun my:mu4e-use-org-for-diary (func path what docid param)
        "If WHAT is \"diary\", import event into org. Otherwise call mu4e~view-temp-handler."
        (if (string= what "diary")
            (progn
              (require 'org-caldav)
              (my:import-ics-to-org path))
          (funcall func path what docid param)))

      (advice-add 'mu4e~view-temp-handler :around #'my:mu4e-use-org-for-diary)

      (with-eval-after-load "message"
        (with-eval-after-load "mu4e-view"
          (define-key mu4e-view-mode-map
            (vector 'remap 'beginning-of-buffer) 'my:message-goto-top)
          (define-key mu4e-view-mode-map
            (vector 'remap 'end-of-buffer) 'my:message-goto-bottom))))))

(use-package message
  :defer t
  :config
  (progn
    (defun my:message-goto-top ()
      (interactive)
      (let ((old-position (point)))
        (message-goto-body)
        (when (equal (point) old-position)
          (beginning-of-buffer))))

    (define-key message-mode-map
      (vector 'remap 'beginning-of-buffer) 'my:message-goto-top)

    (defun my:message-goto-bottom ()
      (interactive)
      (let ((old-position (point))
            (message-position (save-excursion (message-goto-body) (point))))
        (end-of-buffer)
        (when (re-search-backward "^-- $" message-position t)
          (previous-line))
        (when (equal (point) old-position)
          (end-of-buffer))))

    (define-key message-mode-map
      (vector 'remap 'end-of-buffer) 'my:message-goto-bottom)))

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

    (defun my:setup-imenu-for-use-package ()
      "Recognize `use-package` in imenu"
      (when (string= buffer-file-name (expand-file-name "init.el" "~/.emacs.d"))
        (add-to-list 'imenu-generic-expression
                     '("Used Packages"
                       "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2))))
    (add-hook 'emacs-lisp-mode-hook 'my:setup-imenu-for-use-package)

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
  :defer t
  :diminish git-auto-commit-mode)

(use-package pos-tip)

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

(defun update-pillar-image ()
  (interactive)
  (require 'f)
  (require 's)
  (require 'dash)

  (let* ((images-dir "~/Documents/smalltalk/images/")
         (pillar-directories
          (-map #'f-filename (f-directories images-dir
                                            (lambda (dir)
                                              (s-starts-with? "pillar" (f-filename dir)))
                                            t)))

         (pillar-directory (cond
                            ((null pillar-directories)
                             (error "No pillar image found in %s" images-dir))
                            ((= 1 (length pillar-directories))
                             (car pillar-directories))
                            (t (completing-read "Image: " pillar-directories nil t nil nil
                                                (car pillar-directories)))))
         (pillar-image (f-expand (s-concat pillar-directory ".image")
                                 (f-expand pillar-directory images-dir)))
         (pillar-changes (s-concat (f-no-ext pillar-image) ".changes"))
         (destination-image (f-expand "Pharo.image" default-directory))
         (destination-changes (f-expand "Pharo.changes" default-directory)))
    (unless (f-exists? pillar-image)
      (error "No image at %s" pillar-image))
    (unless (f-exists? pillar-changes)
      (error "No changes at %s" pillar-changes))
    (when (or (f-exists? destination-image)
              (f-symlink? destination-image))
      (if (yes-or-no-p (format "Are you sure you want to delete %s? "
                               destination-image))
          (progn
            (f-delete destination-image)
            (f-delete destination-changes))
        (error "Can't continue without deleting existing image %s"
               destination-image)))
    (f-symlink pillar-image destination-image)
    (f-symlink pillar-changes destination-changes)))

(use-package visible-mark
  :config
  (progn
    (global-visible-mark-mode 1)
    (setq visible-mark-max 2)
    (setq visible-mark-faces `(visible-mark-face1 visible-mark-face2))
    (defface visible-mark-active
      '((((type tty) (class mono)))
        (t (:background "magenta"))) "")))

(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)))

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

(use-package names-dev)

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

(add-to-list 'load-path "~/.emacs.d/packages")
(use-package youtube)

(use-package wgrep
  :init
  (progn
    (with-eval-after-load "helm-grep"
      (bind-key  "C-x C-q" #'wgrep-change-to-wgrep-mode helm-grep-mode-map)
      (bind-key "C-c C-c" #'wgrep-finish-edit helm-grep-mode-map))))

(use-package ical2org
  :demand t
  :init
  (progn
    (defun ical2org/buffer-to-agenda (&optional buffer)
      "Import ical events from file `FNAME' to agenda file (will be prompted).
Saves when `NOSAVE' is non-nil."
      (interactive)
      (let ((agenda-file "~/Documents/configuration/org/events.org")
            (events (ical2org/import-buffer (or buffer (current-buffer)))))
        (save-current-buffer
          (find-file agenda-file)
          (goto-char (point-max))
          (newline)
          (dolist (e events)
            (insert (ical2org/format e))
            (newline))
          (save-buffer))))))

(use-package password-store
  :config
  (progn
    (setq password-store-password-length 16)))

(use-package ace-window
  :demand t
  :bind (("M-o" . ace-window))
  :config
  (progn
    (ace-window-display-mode)
    ;; Use same keys for both ace-window and avy
    (setq aw-keys avy-keys)))

(use-package hydra
  :config
  (progn
    (defhydra hydra-goto-line (goto-map ""
                                        :pre (linum-mode 1)
                                        :post (linum-mode -1))
      "goto-line"
      ("g" goto-line "go")
      ("m" set-mark-command "mark" :bind nil)
      ("q" nil "quit"))

    (defhydra hydra-window (:color red :hint nil)
      "
 Split: _v_ert _x_:horz
Delete: _o_nly  _da_ce  _dw_indow  _db_uffer  _df_rame
  Move: _s_wap
Frames: _f_rame new  _df_ delete
  Misc: _m_ark _a_ce  _u_ndo  _r_edo"
      ("h" windmove-left)
      ("j" windmove-down)
      ("k" windmove-up)
      ("l" windmove-right)
      ("H" hydra-move-splitter-left)
      ("J" hydra-move-splitter-down)
      ("K" hydra-move-splitter-up)
      ("L" hydra-move-splitter-right)
      ("|" (lambda ()
             (interactive)
             (split-window-right)
             (windmove-right)))
      ("_" (lambda ()
             (interactive)
             (split-window-below)
             (windmove-down)))
      ("v" split-window-right)
      ("x" split-window-below)
                                        ;("t" transpose-frame "'")
      ("u" winner-undo)
      ("r" winner-redo) ;;Fixme, not working?
      ("o" delete-other-windows :exit t)
      ("a" ace-window :exit t)
      ("f" new-frame :exit t)
      ("s" ace-swap-window)
      ("da" ace-delete-window)
      ("dw" delete-window)
      ("db" kill-this-buffer)
      ("df" delete-frame :exit t)
      ("q" nil)
                                        ;("i" ace-maximize-window "ace-one" :color blue)
                                        ;("b" ido-switch-buffer "buf")
      ("m" headlong-bookmark-jump))
    (global-set-key (kbd "C-. w") #'hydra-window/body)

    (use-package projectile
      :config
      (progn

        (defhydra hydra-projectile-other-window (:color teal)
          "projectile-other-window"
          ("f"  projectile-find-file-other-window        "file")
          ("g"  projectile-find-file-dwim-other-window   "file dwim")
          ("d"  projectile-find-dir-other-window         "dir")
          ("b"  projectile-switch-to-buffer-other-window "buffer")
          ("q"  nil                                      "cancel" :color blue))

        (use-package helm-ag
          :config
          (progn
            (defun my:projectile-ag ()
              (interactive)
              (helm-ag (projectile-project-root)))))

        (defhydra hydra-projectile (:color teal :hint nil)
          "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
_s-f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim       _g_: update gtags      _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir

"
          ("a"   my:projectile-ag)
          ("b"   projectile-switch-to-buffer)
          ("c"   projectile-invalidate-cache)
          ("d"   projectile-find-dir)
          ("fF" projectile-find-file)
          ("ff"  projectile-find-file-dwim)
          ("fd"  projectile-find-file-in-directory)
          ("g"   ggtags-update-tags)
          ("s-g" ggtags-update-tags)
          ("i"   projectile-ibuffer)
          ("K"   projectile-kill-buffers)
          ("m"   projectile-multi-occur)
          ("o"   projectile-multi-occur)
          ("p"   projectile-switch-project "switch project")
          ("s"   projectile-switch-project)
          ("r"   projectile-recentf)
          ("x"   projectile-remove-known-project)
          ("X"   projectile-cleanup-known-projects)
          ("z"   projectile-cache-current-file)
          ("`"   hydra-projectile-other-window/body "other window")
          ("q"   nil "cancel" :color blue))

        (global-set-key (kbd "C-. ;") #'hydra-projectile/body)))))

(add-to-list 'load-path "~/.emacs.d/packages/org-caldav")
(use-package org-caldav
  :bind
  (("C-. o S"   . org-caldav-sync))
  :config
  (progn
    (setq org-caldav-url "https://www.schplaf.org/baikal/cal.php/calendars/damiencnsa"
          org-caldav-calendar-id "default"
          org-caldav-inbox org-default-calendar-file
          org-caldav-files '()
          org-icalendar-timezone "Europe/Berlin"
          org-caldav-save-directory "~/.emacs.d/cache/org-caldav"
          org-caldav-sync-changes-to-org 'all)

    (defun my:import-ics-to-org (path)
      (with-current-buffer (get-buffer-create "*import-ics-to-org*")
        (delete-region (point-min) (point-max))
        (insert-file-contents path)
        (let ((event (org-caldav-convert-event)))
          (with-current-buffer (find-file-noselect (org-caldav-inbox-file org-caldav-inbox))
            (let* ((point-and-level (org-caldav-inbox-point-and-level org-caldav-inbox))
                   (point (car point-and-level))
                   (level (cdr point-and-level)))
              (goto-char point)
              (apply #'org-caldav-insert-org-entry
                     (append event (list nil level))))))))))

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

(add-to-list 'load-path "~/.emacs.d/packages/beginend")
(use-package beginend
  :diminish '(beginend-dired-mode beginend-message-mode)
  :config
  (progn
    (beginend-setup-all)))

(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

(add-to-list 'load-path "~/.emacs.d/packages/pycarddavel")
(use-package pycarddavel
  :bind (("C-. c" . pycarddavel-search-with-helm)))

(add-to-list 'load-path "~/.emacs.d/packages/auth-password-store")
(use-package auth-password-store
  :demand t
  :init
  (progn
    (setq auth-sources '(password-store))))

;;; Emacs Configuration
;; Local Variables:
;; eval: (outline-minor-mode)
;; End:
