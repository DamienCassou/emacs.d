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
 '(magit-repo-dirs (quote ("~/Documents/writing" "~/Documents/candidatures" "~/Documents" "~/.emacs.d/el-get" "~/.emacs.d/themes" "~/Documents/smalltalk" "~/tmp" "~/Documents/projects" "~/Documents/websites" "~/Documents/teaching" "~/")))
 '(magit-repo-dirs-depth 1)
 '(menu-bar-mode nil)
 '(message-log-max t)
 '(next-screen-context-lines 5)
 '(notmuch-labeler-hide-known-labels t)
 '(org-imenu-depth 1)
 '(org-mobile-directory "~/Dropbox/MobileOrg/")
 '(org-mobile-inbox-for-pull "~/Documents/configuration/org/from-mobile.org")
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
 '(user-mail-address "damien.cassou@gmail.com")
 '(vc-follow-symlinks nil)
 '(vc-make-backup-files t)
 '(version-control t)
 '(visible-bell t)
 '(which-function-mode t)
 '(winner-mode t nil (winner) "Use C-c <left|right> to go back to previous windows configuration"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/usr/share/emacs/site-lisp")

(defun darwinp ()
  (interactive)
  "Return true if system is darwin-based (Mac OS X)"
  (string-equal system-type "darwin"))

;; Path
(defun add-to-executable-path (path)
  (let ((expanded-path (expand-file-name path)))
    (add-to-list 'exec-path expanded-path)
    (setenv "PATH" (concat expanded-path ":" (getenv "PATH")))))

(mapcar
 'add-to-executable-path
 (if (darwinp)
     (list  "/usr/local/bin"
            "/usr/local/sbin"
            "~/usr/apps/texlive/latest/bin/universal-darwin/")
   (list "~/Documents/configuration/scripts/"
         "~/usr/apps/texlive/latest/bin/i386-linux/"
         "~/usr/bin")))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (if (not (executable-find "git"))
      (warn "Warning: Git is not available. As a result I can't install el-get")
    (with-current-buffer
        (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
      (let (el-get-master-branch)
        (goto-char (point-max))
        (eval-print-last-sexp)))))

(load "~/.emacs.d/el-get/.loaddefs.el")
(eval-after-load 'el-get
  '(progn
     (push "~/.emacs.d/el-get-recipes" el-get-recipe-path)
     (let ((my-packages   '(
                            ;; Asynchronous file management in Emacs
                            ;; async
                            ;; [M-y] to show kill ring
                            browse-kill-ring
                            ;; Shorter mode names in the modeline
                            diminish
                            ;; improvements to dired+
                            dired+
                            ;; toggle details (size, owner, group,
                            ;; permissions, ...) in dired buffers
                            ;; using <)>
                            dired-details
                            ;; <C-x s> to toggle sudo
                            dired-toggle-sudo
                            ;; M-<up|down|left|right> to move things
                            ;; around
                            drag-stuff
                            ;; el-get itself (useful for info)
                            el-get
                            ;; Correctly takes care of trailing spaces
                            ethan-wspace
                            ;; <C-x => repeadly to mark regions
                            expand-region
                            ;; <z> in dired to facilitate extraction of archive files
                            extractor
                            ;; M-x `fci-mode' to show the right margin
                            fill-column-indicator
                            ;; <M-i> to list sections of the buffer
                            idomenu
                            ;; Install ido everywhere
                            ido-ubiquitous
                            ;; Fix Java mode for annotations
                            java-mode-indent-annotations
                            ;; Major mode for markdown format
                            markdown-mode
                            ;; Control multiple cursors with C-S-c C-S-c
                            multiple-cursors
                            ;; Calendars and todo
                            org-mode
                            ;; More completion for eshell
                            pcomplete-plugins
                            ;; Major mode for Pier-formatted text files
                            pier-mode
                            ;; Get a nice mode line
                            powerline
                            ;; Major mode for python
                            python-mode
                            ;; Switch between shells with <C-'> and
                            ;; C-' followed by more quotes
                            shell-switcher
                            ;;  Replacement for <M-x>
                            smex
                            ;; Displays numbers when using <C-x o>
                            switch-window
                            ;; Assistant to correct english text
                            textlint
                            ;; <C-x u> to show the undo tree
                            undo-tree
                            )))

       (when (executable-find "notmuch")
         ;; Mail client <M-x notmuch>
         (add-to-list 'my-packages 'notmuch)
         ;; Labeler for notmuch
         (add-to-list 'my-packages 'notmuch-labeler))

       (when (executable-find "makeinfo")
         ;; Integrate git <C-x g>
         (add-to-list 'my-packages 'magit))

       (when (executable-find "mpd")
         ;; Listening to music from Emacs with <F11>
         (add-to-list 'my-packages 'mpdel)
         (add-to-list 'my-packages 'emms))

       (when (executable-find "latex")
         (add-to-list 'my-packages 'auctex)
         (add-to-list 'my-packages 'reftex))

       (el-get 'sync my-packages))))

(load "general")
(require 'org)

(server-start)
