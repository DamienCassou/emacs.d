;;; -*- Mode: Emacs-Lisp -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-section-hook
   (quote
    (LaTeX-section-heading LaTeX-section-title LaTeX-section-section)))
 '(LaTeX-verbatim-environments (quote ("verbatim" "verbatim*" "lstlisting")))
 '(TeX-PDF-mode t)
 '(TeX-auto-save t)
 '(TeX-default-unit-for-image "\\linewidth")
 '(TeX-master nil)
 '(TeX-parse-self t)
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
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
 '(el-get-byte-compile nil)
 '(el-get-github-default-url-type (quote https))
 '(el-get-user-package-directory "~/.emacs.d/init-packages")
 '(enable-local-variables :all)
 '(enable-recursive-minibuffers t)
 '(eshell-buffer-shorthand t)
 '(eshell-cmpl-cycle-completions nil)
 '(eval-expression-print-length 20)
 '(eval-expression-print-level nil)
 '(flx-ido-threshhold 10000)
 '(flyspell-tex-command-regexp
   "\\(\\(begin\\|end\\)[ 	]*{\\|\\(cite[a-z*]*\\|label\\|ct\\|c?cauthor\\|sigle\\|\\(lst\\)?\\(lignesa\\|lignes\\|ligne\\)\\|nocheck\\|macitation\\|enword\\|ref\\|eqref\\|pageref\\|page\\|listing\\|usepackage\\|documentclass\\)[ 	]*\\(\\[[^]]*\\]\\)?{[^{}]*\\)")
 '(flyspell-use-meta-tab nil)
 '(font-latex-match-bold-command-keywords
   (quote
    (("damien" "{")
     ("dc" "{")
     ("eb" "{")
     ("bb" "{")
     ("cc" "{")
     ("jl" "{")
     ("sd" "{")
     ("dp" "{")
     ("nl" "{"))))
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
 '(haskell-hoogle-command "hoogle")
 '(icomplete-mode t)
 '(ido-confirm-unique-completion t)
 '(ido-create-new-buffer (quote never))
 '(ido-enable-flex-matching t)
 '(ido-enabled (quote both) t (ido))
 '(ido-everywhere nil nil nil "Better implemented in ido-ubiquitous")
 '(ido-file-extensions-order (quote (".tex" ".el" ".pdf")))
 '(ido-ignore-buffers (quote ("\\` " "^*Back" ".*Completion" "^*Ido")))
 '(ido-ignore-files
   (quote
    ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\`\\.ido\\.last")))
 '(ido-mode (quote both) nil (ido))
 '(ido-ubiquitous-mode t)
 '(ido-use-virtual-buffers t)
 '(imenu-auto-rescan t)
 '(indent-guide-recursive t)
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
 '(magit-commit-signoff t)
 '(magit-repo-dirs
   (quote
    ("~/Documents/writing" "~/Documents/candidatures" "~/Documents" "~/.emacs.d/packages" "~/.emacs.d/themes" "~/Documents/smalltalk" "~/tmp/emacs-configurations" "~/tmp" "~/Documents/projects" "~/Documents/websites" "~/Documents/teaching" "~/")))
 '(magit-repo-dirs-depth 1)
 '(menu-bar-mode nil)
 '(message-log-max t)
 '(mu4e-use-fancy-chars t)
 '(next-screen-context-lines 5)
 '(notmuch-labeler-hide-known-labels t)
 '(org-babel-load-languages (quote ((sh . t) (emacs-lisp . t) (java . t))))
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
 '(proced-filter (quote all))
 '(projectile-keymap-prefix (kbd "C-. p"))
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
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587)
 '(smtpmail-smtp-user "damien.cassou@gmail.com")
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
 '(user-mail-address "damien.cassou@gmail.com")
 '(vc-follow-symlinks nil)
 '(vc-make-backup-files t)
 '(version-control t)
 '(visible-bell t)
 '(winner-mode t nil (winner) "Use C-c <left|right> to go back to previous windows configuration"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-mode-line ((t (:foreground "black" :weight bold))) t)
 '(hl-line ((t (:underline t))))
 '(mode-line ((t (:background "#bd6626" :foreground "#f6f3e8"))))
 '(mode-line-highlight ((t (:foreground unspecified :background unspecified :weight bold))))
 '(mode-line-inactive ((t (:background "#454545" :foreground "#666666"))))
 '(mu4e-header-highlight-face ((t (:underline t))) t)
 '(org-agenda-done ((t (:foreground "LightSalmon" :strike-through t))))
 '(org-done ((t (:foreground "LightSalmon" :strike-through t :weight bold))))
 '(org-headline-done ((t (:foreground "LightSalmon" :strike-through t))))
 '(powerline-active1 ((t (:inherit mode-line :background "#666666" :foreground "#f6f3e8"))))
 '(powerline-active2 ((t (:inherit mode-line :background "#252525" :foreground "#f6f3e8"))))
 '(powerline-inactive2 ((t (:inherit mode-line :background "#666666" :foreground "#aaaaaa"))))
 '(visible-mark-face1 ((t (:background "red" :foreground "black"))))
 '(visible-mark-face2 ((t (:background "salmon" :foreground "black")))))

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(require 'cask "~/.emacs.d/packages/cask/cask.el")
(cask-initialize)

(require 'powerline)
(powerline-default-theme)

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

(use-package-with-elapsed-timer "Starting server"
  (server-start))

(defun my:setup-frame ()
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (when (window-system)
    (load-theme 'niflheim)
    (set-selected-frame-dark)))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (my:setup-frame)))
  (my:setup-frame))



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
         "/var/setuid-wrappers"
         "/home/cassou/.nix-profile/bin"
         "/home/cassou/.nix-profile/sbin"
         "/home/cassou/.nix-profile/lib/kde4/libexec"
         "/nix/var/nix/profiles/default/bin"
         "/nix/var/nix/profiles/default/sbin"
         "/nix/var/nix/profiles/default/lib/kde4/libexec"
         "/run/current-system/sw/bin"
         "/run/current-system/sw/sbin"
         "/run/current-system/sw/lib/kde4/libexec")))
 
(mapc 'add-to-executable-path '("~/.emacs.d/packages/cask/bin"))

(defun suspend-on-tty-only ()
  (interactive)
  (unless window-system
    (suspend-frame)))

(bind-key "C-x C-z" 'suspend-on-tty-only)

(add-to-list 'default-frame-alist '(cursor-type bar . 3))

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

(bind-key "<f5>" 'comment-region)

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

;; faster than C-x o
(bind-key* "M-o" 'other-window)
(unbind-key "C-x o")

;; faster than C-x z
(bind-key "C-z" 'repeat)
(unbind-key "C-x z")

(define-prefix-command 'endless/toggle-map)
(bind-key "t" 'endless/toggle-map ctl-x-map)
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

(use-package dired
  :defer t
  :bind (("C-x C-j" . dired-jump))
  :config
  (progn
    (use-package runner)
    (use-package dired-x)
    (use-package dired-details+
      :disabled t ;; useless in emacs 24.4
      :init
      (progn
        (setq dired-details-hidden-string "")))
    (use-package dired-imenu)

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

    (defun dired-back-to-top ()
      (interactive)
      (goto-char (point-min))
      (dired-next-line (if dired-omit-mode 1 3)))

    (define-key dired-mode-map
      (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

    (defun dired-jump-to-bottom ()
      (interactive)
      (goto-char (point-max))
      (dired-next-line -1))

    (define-key dired-mode-map
      (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

    (defun dired-move-beginning-of-line ()
      (interactive)
      (let ((point (point)))
        (dired-move-to-filename)
        (when (= point (point))
          (move-beginning-of-line nil))))

    (define-key dired-mode-map
      (vector 'remap 'move-beginning-of-line) 'dired-move-beginning-of-line)))

(use-package python
  :config
  (progn
    (defun my:setup-python-mode ()
      (eval-after-load "auto-complete-config"
        `(progn
           (with-eval-after-load "yasnippet" (add-to-list 'ac-sources 'ac-source-yasnippet))
           (add-to-list 'ac-sources 'ac-source-filename))))

    (add-hook 'python-mode-hook 'my:setup-python-mode)))

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
  (progn (add-to-list 'bibtex-BibTeX-entry-alist
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
            (push '("keywords") (fifth list)))
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
  :bind ("C-x g" . magit-status)
  :config
  (progn
    (require 'magit-svn)
    (add-hook 'magit-mode-hook 'turn-on-magit-svn)

    (defun magit-ignore-latex-project ()
      (interactive)
      (mapc
       #'magit-ignore-file
       (list "*.aux" "*.log" "*.out" "*.bbl" "*.blg" "auto/" "*.synctex.gz" "*.toc"))
      (magit-refresh))))

(use-package ido
  :config
  (progn
    (use-package flx-ido
      :init
      (progn
        (ido-mode 1)
        (ido-everywhere 1)
        (flx-ido-mode 1)
        ;; disable ido faces to see flx highlights.
        (setq ido-use-faces t)))
    (use-package ido-vertical-mode
      :init
      (progn
        (ido-vertical-mode 1)))
    (use-package ido-ubiquitous
      :init
      (progn
        (ido-ubiquitous-mode))))
  :init
  (progn
    (defun ido-backquote-to-home ()
      "Press ` to easily go to ~ and ~/.emacs.d"
      ;; Go straight home
      (define-key ido-file-completion-map
        (kbd "`")
        (lambda ()
          (interactive)
          (if (looking-back "~/")
              (insert ".emacs.d/")
            (if (looking-back "/")
                (insert "~/")
              (call-interactively 'self-insert-command))))))
    (add-hook 'ido-setup-hook 'ido-backquote-to-home)
    (ido-mode 'buffer)))

(use-package info
  :bind ("C-h i" . info-other-window)
  :init
  (progn
    ;; Redefines info-other-window to use features of `info'
    (defun info-other-window (&optional file-or-node buffer)
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

(use-package magit-svn
  :defer t
  :diminish magit-svn-mode)

(use-package ispell
  :defer t
  :requires flyspell
  :bind
  (("C-. i b"   . ispell-buffer)
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
      (unless arg
        (ispell-set-dictionary "francais"))
      (flyspell-buffer))

    (defun ispell-change-dictionary-to-english (arg)
      (interactive "P")
      (ispell-change-dictionary "english")
      (unless arg
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

(use-package idomenu
  :bind ("M-i" . idomenu))

(use-package org
  :defer t
  :bind
  (("C-. o t"   . org-capture)
   ("C-. o a"   . org-agenda)
   ("C-. o ,"   . org-cycle-agenda-files)
   ("C-. o SPC" . dc/my-agenda))
  :init
  (progn
    (setq org-modules '(org-publish org-protocol org-capture ox-beamer))

    (eval-after-load "org-latex"
      `(progn
         (setq org-latex-listings t)
         (add-to-list 'org-latex-packages-alist '("" "listings"))
         (add-to-list 'org-latex-packages-alist '("" "color"))))

    ;; Display the agenda
    (defun nico/jump-to-org-agenda ()
      (interactive)
      (dc/my-agenda)
      (delete-other-windows))

    (defun dc/my-agenda ()
      (interactive)
      (require 'org-agenda)
      (let ((entry (assoc " " org-agenda-custom-commands)))
        (org-agenda-run-series (nth 1 entry) (cddr entry))))

    ;; Go to the agenda buffer after 10' idle
    (run-with-idle-timer 600 t 'nico/jump-to-org-agenda)

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
          (quote (("N" "Notes" tags "NOTE"
                   ((org-agenda-overriding-header "Notes")
                    (org-tags-match-list-sublevels t)))
                  ("h" "Habits" tags-todo "STYLE=\"habit\""
                   ((org-agenda-overriding-header "Habits")
                    (org-agenda-sorting-strategy
                     '(todo-state-down effort-up category-keep))))
                  ("l" "Logbook" nico/org-agenda-log ""
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

    (setq org-agenda-files

          '("~/Documents/configuration/org/refile.org"
            "~/Documents/configuration/org/tasks.org"
            "~/Documents/configuration/org/someday.org"
            "~/Documents/configuration/org/repeating.org"))

    (setq org-refile-targets `(("tasks.org"      :maxlevel . 2)
                               ("someday.org"    :maxlevel . 2)
                               ("repeating.org"  :maxlevel . 2)))

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
             "* TODO %?%i\n %a")))

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

    (defun my:open-file-with-dired (file)
      "Ask dired how it would open FILE.
This code is useful if dired is well configured to open files
with external applications. I recommend using the `runner`
package to do so."
      (use-package dired-aux)
      (dired-do-async-shell-command
       (dired-guess-shell-command (format  "Open %s " file) (list file))
       0
       (list file)))

    (add-to-list 'org-file-apps '(t . (my:open-file-with-dired file)))
    (add-to-list 'org-file-apps '("\\.png\\'" . default))))

(use-package calc
  :defer t
  :config
  (defmath vwsum (vec1 vec2)
    "Weighted sum: VEC1 are marks and VEC2 are coefficients."
    (+ (* vec1 vec2))))

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

(use-package browse-kill-ring
  :init
  (progn
    (browse-kill-ring-default-keybindings)))

(use-package drag-stuff
  :diminish drag-stuff-mode
  :init
  (progn
    (drag-stuff-global-mode t)
    (add-to-list 'drag-stuff-except-modes 'org-mode)
    (add-to-list 'drag-stuff-except-modes 'rebase-mode)))

(use-package ethan-wspace
  :disabled t
  :defer t
  :diminish ethan-wspace-mode
  :init
  (progn
    (global-ethan-wspace-mode 1)))

(use-package expand-region
  :bind ("C-x =" . er/expand-region))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package org-pomodoro
  :defer t
  :disabled t
  :init
  (progn
    (eval-after-load "org"
      `(progn
         (bind-key "C-c p" 'org-pomodoro  org-mode-map)))))

(use-package shell-switcher
  :defer t
  :bind (("C-M-'"   . shell-switcher-new-shell)
         ("C-'"     . shell-switcher-switch-buffer)
         ("C-x 4 '" . shell-switcher-switch-buffer-other-window)))

(use-package smex
  :bind ("M-x" . smex)
  :config
  (progn
    (smex-initialize)))

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

(use-package switch-window
  :disabled t
  :bind (("C-x o" . switch-window)))

(use-package guide-key
  :diminish guide-key-mode
  :idle
  (progn
    (setq guide-key/guide-key-sequence '("C-x 4" "C-. p" "C-c @" "C-x t"))
    (guide-key-mode 1)))

(use-package pillar
  :mode ("\\.\\(pier\\|pillar\\)\\'" . pillar-mode)
  :config
  (progn
    (eval-after-load "auto-complete-config"
      `(progn
         (add-to-list 'ac-modes 'pillar-mode)))

    (defun my:setup-pillar-mode ()
      (eval-after-load "auto-complete-config"
        `(progn
           (with-eval-after-load "yasnippet" (add-to-list 'ac-sources 'ac-source-yasnippet))
           (add-to-list 'ac-sources 'ac-source-filename)))
      (variable-pitch-mode))

    (add-hook 'pillar-mode-hook 'my:setup-pillar-mode)))

(use-package projectile
  :diminish projectile-mode
  :init
  (progn
    (projectile-global-mode)))

(use-package gnus-dired
  :disabled t
  :bind (("C-x C-a" . gnus-dired-attach))
  :defer t)

(add-to-list 'load-path "~/.emacs.d/packages/mu/mu4e")
(use-package mu4e
  :disabled t
  :bind (("C-. m m" . mu4e) ("C-. m c" . mu4e-compose-new) ("C-. m i" . mu4e-goto-inbox))
  :init
  (progn
    (setq mu4e-update-interval 30))
  :config
  (progn
    (setq mu4e-mu-binary "~/.emacs.d/packages/mu/mu/mu")
    (setq mu4e-maildir       "~/Mail/Gmail")
    (setq mu4e-drafts-folder "/[Gmail].Drafts")
    (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
    (setq mu4e-trash-folder  "/[Gmail].Trash")
    (setq mu4e-hide-index-messages t)
    (setq mu4e-attachment-dir "~/")
    (setq mu4e-view-show-images t)

    (bind-key "d" 'mu4e-headers-mark-for-delete
              mu4e-headers-mode-map)
    (bind-key "d" 'mu4e-view-mark-for-delete
              mu4e-view-mode-map)

    (add-to-list 'mu4e-view-actions
                 '("View in browser" . mu4e-action-view-in-browser) t)

    ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
    (setq mu4e-sent-messages-behavior 'delete)

    (setq mu4e-maildir-shortcuts
          '( ("/INBOX"               . ?i)
             ("/[Gmail].Sent Mail"   . ?s)
             ("/[Gmail].Drafts"      . ?d)))

    ;; allow for updating mail using 'U' in the main view:
    (setq mu4e-get-mail-command "offlineimap")

    ;; something about ourselves
    (setq
     user-mail-address "damien.cassou@gmail.com"
     user-full-name  "Damien Cassou"
     message-signature t
     message-signature-file "~/.signature-gmail")

    (setq mu4e-user-mail-address-list
          '("damien.cassou@gmail.com" "damien.cassou@lifl.fr" "damien.cassou@inria.fr" "cassou@inria.fr" "damien.cassou@laposte.net" "Damien.Cassou@univ-lille1.fr"))

    ;; sending mail -- replace USERNAME with your gmail username
    ;; also, make sure the gnutls command line utils are installed
    ;; package 'gnutls-bin' in Debian/Ubuntu

    (require 'smtpmail)

    ;; alternatively, for emacs-24 you can use:
    (setq
     message-send-mail-function 'smtpmail-send-it
     smtpmail-stream-type 'starttls
     smtpmail-default-smtp-server "smtp.gmail.com"
     smtpmail-smtp-server "smtp.gmail.com"
     smtpmail-smtp-service 587)

    ;; don't keep message buffers around
    (setq message-kill-buffer-on-exit t)

    (defun mu4e-goto-inbox ()
      (interactive)
      (mu4e~headers-jump-to-maildir "/INBOX"))

    (require 'gnus-dired)

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

    (defun mu4e-compose-goto-top ()
      (interactive)
      (let ((old-position (point)))
        (message-goto-body)
        (when (equal (point) old-position)
          (beginning-of-buffer))))

    (define-key mu4e-compose-mode-map
      (vector 'remap 'beginning-of-buffer) 'mu4e-compose-goto-top)

    (defun mu4e-compose-goto-bottom ()
      (interactive)
      (let ((old-position (point))
            (message-position (save-excursion (message-goto-body) (point))))
        (end-of-buffer)
        (when (re-search-backward "^-- $" message-position t)
          (previous-line))
        (when (equal (point) old-position)
          (end-of-buffer))))

    (define-key mu4e-compose-mode-map
      (vector 'remap 'end-of-buffer) 'mu4e-compose-goto-bottom)))

(use-package image
  :config
  (imagemagick-register-types))

(use-package diminish
  :config
  (progn
    (eval-after-load "simple"
      `(progn
         (diminish 'overwrite-mode)))))

(use-package discover
  :init
  (progn
    (global-discover-mode 1)))

(use-package yasnippet
  :disabled t
  :defer t
  :config
  (progn
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
    (yas-global-mode)
    (yas-reload-all)

    ;; let <tab> be bound to smart-tab
    (define-key yas-minor-mode-map [(tab)] nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)))

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
        (setq imenu-generic-expression
              '((nil "^\\s-*(\\(def\\(?:advice\\|generic\\|ine-\\(?:compiler-macro\\|derived-mode\\|g\\(?:\\(?:eneric\\|lobal\\(?:\\(?:ized\\)?-minor\\)\\)-mode\\)\\|m\\(?:ethod-combination\\|inor-mode\\|odify-macro\\)\\|s\\(?:etf-expander\\|keleton\\)\\)\\|m\\(?:acro\\|ethod\\)\\|s\\(?:etf\\|ubst\\)\\|un\\*?\\)\\|use-package\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2)
                ("Variables" "^\\s-*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\)\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2)
                ("Variables" "^\\s-*(defvar\\s-+\\(\\(\\sw\\|\\s_\\)+\\)[[:space:]\n]+[^)]" 1)
                ("Types" "^\\s-*(\\(def\\(?:class\\|face\\|group\\|ine-\\(?:condition\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\|yp\\)e\\)\\)\\)\\s-+'?\\(\\(\\sw\\|\\s_\\)+\\)" 2)))))

    (add-hook 'emacs-lisp-mode-hook 'my:setup-imenu-for-use-package)

    (defun my:setup-lisp-mode ()
      (eval-after-load "auto-complete-mode"
        `(progn
           (add-to-list 'ac-sources 'ac-source-filename))))

    (add-hook 'emacs-lisp-mode-hook 'my:setup-lisp-mode t)

    (use-package paredit
      :diminish paredit-mode
      :config
      (progn
        (use-package eldoc)
        (eldoc-add-command 'paredit-backward-delete 'paredit-close-round)))

    (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)))

(use-package autorevert
  :defer t
  :diminish auto-revert-mode)

(use-package git-auto-commit-mode
  :defer t
  :diminish git-auto-commit-mode)

(use-package smart-tab
  :init
  (progn
    (global-smart-tab-mode 1)
    (add-to-list 'smart-tab-disabled-major-modes 'help-mode)
    (add-to-list 'smart-tab-disabled-major-modes 'Custom-mode)))

(use-package pos-tip)

(use-package auto-complete-config
  :disabled t
  :init
  (progn
    (use-package pos-tip)
    (ac-config-default))
  :config
  (progn
    (add-to-list 'ac-user-dictionary user-mail-address)
    (setq ac-use-menu-map t)
    (ac-flyspell-workaround)))

(add-to-list 'load-path "~/.emacs.d/packages/skeletor.el")
(use-package skeletor
  :commands (skeletor-create-project))

(use-package sh-script
  :defer t
  :config
  (progn
    (defun my:setup-sh-mode ()
      (eval-after-load "auto-complete-config"
        `(progn
           (add-to-list 'ac-sources 'ac-source-filename))))
    (add-hook 'sh-mode-hook 'my:setup-sh-mode t)))

(use-package smartscan
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook 'smartscan-mode)
    (add-hook 'pillar-mode-hook 'smartscan-mode)))

(use-package company
  :disabled t
  :init
  (progn
    (add-hook 'prog-mode-hook 'company-mode)
    (with-eval-after-load "yasnippet"
      (bind-key "C-. y" 'company-yasnippet))))

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
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)))

(use-package paren-face
  :init
  (progn
    (global-paren-face-mode)))

(use-package ido-at-point
  :init
  (progn
    (ido-at-point-mode)))

(use-package find-func
  :config
  (progn
    (define-key 'help-command (kbd "C-l") 'find-library)
    (define-key 'help-command (kbd "C-f") 'find-function)
    (define-key 'help-command (kbd "C-k") 'find-function-on-key)
    (define-key 'help-command (kbd "C-v") 'find-variable)))

(use-package hungry-delete
  :disabled t
  :config
  (global-hungry-delete-mode))

(use-package git-timemachine)

(use-package noccur)

(use-package anzu
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :diminish anzu-mode
  :config
  (progn
    (global-anzu-mode +1)))

(use-package indent-guide
  :disabled t
  :config
  (progn
    (indent-guide-global-mode)))

;;; Emacs Configuration
;; Local Variables:
;; eval: (outline-minor-mode)
;; End:
