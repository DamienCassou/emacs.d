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
 '(TeX-view-program-list (quote (("AcrobatReader" "acroread %o"))))
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "AcrobatReader") (output-html "xdg-open"))))
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.saves"))))
 '(bibtex-align-at-equal-sign t)
 '(bibtex-autokey-name-case-convert-function (quote identity))
 '(bibtex-autokey-name-length 4)
 '(bibtex-autokey-titlewords 0)
 '(bibtex-entry-format (quote (opts-or-alts required-fields whitespace realign last-comma delimiters)))
 '(bookmark-save-flag 1)
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(confirm-kill-emacs (quote y-or-n-p))
 '(current-language-environment "UTF-8")
 '(custom-safe-themes t)
 '(default-frame-alist (quote ((cursor-type bar . 3) (left . 10) (top . 10) (font . "Inconsolata-12"))))
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
 '(el-get-user-package-directory "~/.emacs.d/init-packages")
 '(enable-local-variables :all)
 '(eval-expression-print-level 7)
 '(flyspell-tex-command-regexp "\\(\\(begin\\|end\\)[ 	]*{\\|\\(cite[a-z*]*\\|label\\|ct\\|c?cauthor\\|sigle\\|\\(lst\\)?\\(lignesa\\|lignes\\|ligne\\)\\|nocheck\\|macitation\\|enword\\|ref\\|eqref\\|pageref\\|page\\|listing\\|usepackage\\|documentclass\\)[ 	]*\\(\\[[^]]*\\]\\)?{[^{}]*\\)")
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
 '(ido-use-virtual-buffers t)
 '(imenu-auto-rescan t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(initial-scratch-message nil)
 '(io-command "/home/cassou/Downloads/stevedekorte-io-f641230/build/_build/binaries/io")
 '(kept-new-versions 6)
 '(magit-commit-signoff t)
 '(magit-repo-dirs (quote ("~/Documents/writing" "~/Documents/candidatures/" "~/Documents/" "~/.emacs.d/el-get/" "~/.emacs.d/themes/" "~/Documents/smalltalk/SBA/" "~/tmp/")))
 '(magit-repo-dirs-depth 1)
 '(menu-bar-mode nil)
 '(org-agenda-files "~/Documents/configuration/org/agenda_files")
 '(org-file-apps (quote ((auto-mode . emacs) ("\\.x?html?\\'" . default) ("\\.pdf\\'" . "/usr/bin/acroread %s") (t . "/usr/bin/gnome-open %s"))))
 '(org-hide-leading-stars t)
 '(org-log-done t)
 '(org-special-ctrl-a/e t)
 '(org-time-stamp-rounding-minutes 10)
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
 '(shell-switcher-new-shell-function (quote shell-switcher-make-eshell))
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(smex-save-file "~/.config/smex-items")
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

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; Path
(mapcar
 (lambda (path)
   (let ((expanded-path (expand-file-name path)))
     (add-to-list 'exec-path expanded-path)
     (setenv "PATH" (concat expanded-path ":" (getenv "PATH")))))
 '("~/Documents/configuration/scripts/"
   "~/usr/apps/texlive/latest/bin/i386-linux/"))

(unless (require 'el-get nil t)
  (if (not (executable-find "git"))
      (warn "Warning: Git is not available. As a result I can't install el-get")
    (with-current-buffer
	(url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
      (let ((el-get-install-branch "3.stable"))
	(goto-char (point-max))
	(eval-print-last-sexp)))))

(eval-after-load 'el-get
  '(progn
     (push "~/.emacs.d/el-get-recipes" el-get-recipe-path)
     (setq el-get-sources '(
			    ;; Asynchronous file management in Emacs
			    ;; async
			    ;; [M-y] to show kill ring
			    browse-kill-ring
			    ;; Shorter mode names in the modeline
			    diminish
			    ;; toggle details (size, owner, group,
			    ;; permissions, ...) in dired buffers
			    ;; using <)>
			    dired-details
			    ;; <C-x s> to toggle sudo
			    dired-toggle-sudo
			    ;; M-<up|down|left|right> to move things
			    ;; around
			    drag-stuff
			    ;; Music player, start with <f12>
			    emms
			    ;; <M-@> repeadly to mark regions
			    expand-region
			    ;; `fci-mode' to show the right margin
			    fill-column-indicator
			    ;; <M-i> to list sections of the buffer
			    idomenu
			    ;; Fix Java mode for annotations
			    java-mode-indent-annotations
			    ;; Integrate git <C-x g>
			    magit
			    ;; Major mode for markdown format
			    markdown-mode
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
			    ))

     (when (executable-find "latex")
       (add-to-list 'el-get-sources 'auctex)
       (add-to-list 'el-get-sources 'reftex))

     ;; (when (executable-find "sbcl")
     ;;   (add-to-list 'el-get-sources 'slime))

     (el-get 'sync el-get-sources)))

(load "general")
