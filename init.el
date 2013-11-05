;;; -*- Mode: Emacs-Lisp -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-section-hook (quote (LaTeX-section-heading LaTeX-section-title LaTeX-section-section)))
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
 '(bibtex-entry-format (quote (opts-or-alts required-fields whitespace realign last-comma delimiters)))
 '(bookmark-save-flag 1)
 '(calendar-date-style (quote european))
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(confirm-kill-emacs (quote y-or-n-p))
 '(current-language-environment "UTF-8")
 '(custom-safe-themes t)
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
 '(flyspell-tex-command-regexp "\\(\\(begin\\|end\\)[ 	]*{\\|\\(cite[a-z*]*\\|label\\|ct\\|c?cauthor\\|sigle\\|\\(lst\\)?\\(lignesa\\|lignes\\|ligne\\)\\|nocheck\\|macitation\\|enword\\|ref\\|eqref\\|pageref\\|page\\|listing\\|usepackage\\|documentclass\\)[ 	]*\\(\\[[^]]*\\]\\)?{[^{}]*\\)")
 '(flyspell-use-meta-tab nil)
 '(font-latex-match-bold-command-keywords (quote (("damien" "{") ("dc" "{") ("eb" "{") ("bb" "{") ("cc" "{") ("jl" "{") ("sd" "{") ("dp" "{") ("nl" "{"))))
 '(font-latex-match-math-command-keywords (quote (("mm" "{") ("contract" "{{{"))))
 '(font-latex-match-reference-keywords (quote (("ccauthor" "[{") ("cauthor" "{"))))
 '(font-latex-match-type-command-keywords (quote (("ct" "{") ("method" "{") ("class" "{") ("lct" "{"))))
 '(frame-title-format "Emacs: %b" t)
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
 '(ido-file-extensions-order (quote (".tex" ".el" ".pdf")))
 '(ido-ignore-buffers (quote ("\\` " "^*Back" ".*Completion" "^*Ido")))
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\`\\.ido\\.last")))
 '(ido-max-prospects 6)
 '(ido-mode (quote both) nil (ido))
 '(ido-ubiquitous-mode t)
 '(ido-use-virtual-buffers t)
 '(imenu-auto-rescan t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(initial-scratch-message nil)
 '(io-command "/home/cassou/Downloads/stevedekorte-io-f641230/build/_build/binaries/io")
 '(kept-new-versions 6)
 '(kept-old-versions 10)
 '(magit-commit-signoff t)
 '(magit-repo-dirs (quote ("~/Documents/writing" "~/Documents/candidatures" "~/Documents" "~/.emacs.d/packages" "~/.emacs.d/themes" "~/Documents/smalltalk" "~/tmp" "~/Documents/projects" "~/Documents/websites" "~/Documents/teaching" "~/")))
 '(magit-repo-dirs-depth 1)
 '(menu-bar-mode nil)
 '(message-log-max t)
 '(next-screen-context-lines 5)
 '(notmuch-labeler-hide-known-labels t)
 '(org-clock-clocked-in-display nil)
 '(org-completion-use-ido t)
 '(org-default-notes-file "tasks.org")
 '(org-directory "~/Documents/configuration/org")
 '(org-fontify-done-headline t)
 '(org-hide-leading-stars t)
 '(org-imenu-depth 1)
 '(org-log-done (quote time))
 '(org-special-ctrl-a/e t)
 '(org-time-stamp-rounding-minutes (quote (10 10)))
 '(proced-filter (quote all))
 '(read-file-name-completion-ignore-case t)
 '(recentf-auto-cleanup 300)
 '(recentf-exclude (quote ("~$" "\\.log$")))
 '(recentf-max-saved-items 4000)
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/recentf")
 '(reftex-plug-into-AUCTeX t)
 '(reftex-view-crossref-cite-macros "\\`\\\\cite\\|cite\\*?\\'\\|bibentry\\|ccauthor")
 '(reftex-view-crossref-extra nil)
 '(report-emacs-bug-no-explanations t)
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/places")
 '(scheme-program-name "petite")
 '(scroll-bar-mode nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(sentence-end-double-space nil)
 '(sh-indent-comment t)
 '(shell-switcher-mode t)
 '(shell-switcher-new-shell-function (quote shell-switcher-make-eshell))
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
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
 '(undo-tree-mode-lighter "")
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
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
 '(org-agenda-done ((t (:foreground "LightSalmon" :strike-through t))))
 '(org-done ((t (:foreground "LightSalmon" :strike-through t :weight bold))))
 '(org-headline-done ((t (:foreground "LightSalmon" :strike-through t)))))

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(require 'cask "~/.emacs.d/packages/cask/cask.el")
(cask-initialize)

(require 'bind-key "~/.emacs.d/packages/use-package/bind-key.el")
(require 'use-package "~/.emacs.d/packages/use-package/use-package.el")
(eval-when-compile
  (setq use-package-verbose (null byte-compile-current-file)))

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
         "~/usr/apps/texlive/latest/bin/i386-linux/"
         "~/usr/bin")))

(defun suspend-on-tty-only ()
  (interactive)
  (unless window-system
    (suspend-frame)))

(bind-key "C-z" 'suspend-on-tty-only)
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

(use-package dired
  :defer t
  :bind ("C-x C-j" . dired-jump)
  :config
  (progn
    (use-package runner)
    (use-package dired-x)
    (use-package dired-details+
      :init
      (progn
        (setq dired-details-hidden-string "")))

    (when (darwinp)
      ;; Use coreutils from homebrew to provide a real ls
      (setq dired-use-ls-dired t)
      (setq insert-directory-program "gls"))

    (let ((extensions-to-ignore '(".out" ".lol" ".ali" ".upload" ".changes" ".build" ".dsc")))
      (mapc (lambda (extension)
              (add-to-list 'completion-ignored-extensions extension)
              (add-to-list 'dired-omit-extensions extension))
            extensions-to-ignore))

    (let ((files-to-ignore '("Thumbs\.db" "Thumbs\.db:encryptable" "b~.*\.ad[sb]")))
      (mapc (lambda (filename)
                (setq dired-omit-files
                      (concat dired-omit-files "\\|^" filename "$")))
              files-to-ignore))

    (add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))

    (defun dired-back-to-top ()
      (interactive)
      (goto-char (point-min))
      (dired-next-line (if dired-omit-mode 2 4)))

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
      (case type
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
    (add-hook 'ido-setup-hook 'ido-backquote-to-home)))

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

(use-package magit-svn
  :defer t
  :diminish magit-svn-mode)

(use-package ispell
  :defer t
  :requires flyspell
  :bind 
  (("C-. i b"   . ispell-buffer)
   ("C-. i w"   . ispell-word)
   ("C-. i d f" . ispell-change-dictionary-to-french)
   ("C-. i d e" . ispell-change-dictionary-to-english)
   ("C-. i d ?" . ispell-change-dictionary))

  :init
  (progn
    (defun ispell-set-dictionary (dict)
      (save-excursion
        (add-file-local-variable 'ispell-local-dictionary dict)))

    (defun ispell-change-dictionary-to-french ()
      (interactive)
      (ispell-change-dictionary "francais")
      (ispell-set-dictionary "francais")
      (flyspell-buffer))

    (defun ispell-change-dictionary-to-english ()
      (interactive)
      (ispell-change-dictionary "english")
      (ispell-set-dictionary "english")
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

(use-package ethan-wspace
  :defer t
  :diminish ethan-wspace-mode)

(use-package eldoc
  :defer t
  :diminish eldoc-mode)

(use-package checkdoc
  :defer t
  :diminish checkdoc-minor-mode)

(use-package face-remap
  :defer t
  :diminish text-scale-mode)

(use-package flycheck
  :defer t
  :diminish flycheck-mode)

(use-package idomenu
  :bind ("M-i" . idomenu))

(use-package org
  :defer t
  :bind
  (("C-. o t" . org-capture)
   ("C-. o a" . org-agenda)
   ("C-. o ," . org-cycle-agenda-files))
  :init
  (progn
    (setq org-modules '(org-bbdb org-bibtex org-docview org-gnus
                                 org-habit org-info org-jsinfo
                                 org-habit org-irc org-mew
                                 org-mhe org-protocol org-rmail
                                 org-vm org-wl org-w3m
                                 org-bookmark org-pomodoro
                                 org-pomodoro-pidgin
                                 org-publish))

    ;; Display the agenda
    (defun nico/jump-to-org-agenda ()
      (interactive)
      (let ((buffer (get-buffer "*Org Agenda*")))
        (if buffer
            (switch-to-buffer buffer)
          (org-agenda-list))
        (delete-other-windows)))

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
    (setq org-agenda-files

          '("~/Documents/configuration/org/tasks.org"
            "~/Documents/configuration/org/someday.org"
            "~/Documents/configuration/org/repeating.org"))

    (setq org-refile-targets `(("tasks.org"      :maxlevel . 2)
                               ("someday.org"    :maxlevel . 2)
                               ("repeating.org"  :level    . 1)))

    (setq org-todo-keywords
          '((sequence "TODO(t)"    "|" "DONE(d)" "CANCELLED(c)")
            (sequence "APPT(p)"    "|" "DONE(d)" "CANCELED(c)")
            (sequence "WAITING(w)" "|" "DONE(d)")))

    (setq org-todo-keyword-faces
          '(("NEXT" :foreground "orange" :weight bold)
            ("CANCELLED" :foreground "forest green")))

    (setq org-capture-templates '())
    (add-to-list
     'org-capture-templates
     '("t" "Todo [inbox]" entry
       (file+headline org-default-notes-file "Tasks")
       "* TODO %?%i\n %a"))

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
              (message "Can't compile less file %s. %s" filename output))))))))

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
  :defer t
  :config
  (progn
    (load "mybibtex" t t t)
    (eval-after-load "tex"
      '(progn
         (add-to-list 'TeX-command-list
                      '("Bibtex all" "multibib/bibtexall" TeX-run-BibTeX
                        nil t :help "Run Bibtex on all aux files") t)
         (unless (darwinp)
           (progn
             (add-to-list 'TeX-view-program-list '("AcrobatReader" "acroread %o"))
             (add-to-list 'TeX-view-program-selection '(output-pdf "AcrobatReader"))))))

    (eval-after-load "latex"
      `(progn
         (defun LaTeX-align-table ()
           (interactive)
           (save-excursion
             (LaTeX-mark-environment)
             (while (re-search-forward "& *" (region-end) t)
               (replace-match "& " nil nil))
             (LaTeX-mark-environment)
             (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)\\(&\\|\\\\\\\\\\)" 1 1 t)))
         (define-key (eval 'TeX-mode-map) (kbd "s-a") #'LaTeX-align-table)))

    (if (darwinp)
        (require 'auctex-skim-sync)
      (require 'auctex-evince-sync))))

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
    (global-undo-tree-mode)))

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
