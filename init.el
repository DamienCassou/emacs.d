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
 '(default-frame-alist (quote ((cursor-type bar . 3) (left . 10) (top . 10) (font . "-unknown-dejavu sans mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"))))
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t)
 '(diff-switches "-u")
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
 '(safe-local-variable-values (quote ((TeX-master . \.\./main) (eval whitespace-mode) (eval set-face-attribute (quote whitespace-line) nil :background "red1" :foreground "yellow" :weight (quote bold)) (eval set-face-attribute (quote whitespace-tab) nil :background "red1" :foreground "yellow" :weight (quote bold)) (whitespace-style face trailing lines-tail) (whitespace-line-column . 80) (eval require (quote whitespace)) (sh-basic-offset . 4) (TeX-master . Lint\.tex) (sh-indent-comment . t) (default-justification quote full) (LocalWords . ADL) (reftex-default-bibliography "../Bibliography.bib") (reftex-default-bibliography quote ("../Bibliography.bib")) (reftex-default-bibliography . "../Bibliography.bib") (TeX-engine . unsafelatex) (eval defun byte-compile-this-file nil (write-region (point-min) (point-max) buffer-file-name nil (quote t)) (byte-compile-file buffer-file-name) nil) (TeX-master . flyer) (TeX-master . t) (TeX-PDF-mode . t))))
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/places")
 '(scheme-program-name "petite")
 '(scroll-bar-mode nil)
 '(sentence-end-double-space nil)
 '(sh-indent-comment t)
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(smex-save-file "~/.config/smex-items")
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
 '(winner-mode t nil (winner)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path "~/.emacs.d")

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (if (not (executable-find "git"))
      (warn "Warning: Git is not available. As a result I can't install el-get")
    (with-current-buffer
	(url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
      (end-of-buffer)
      (eval-print-last-sexp))))

(eval-after-load 'el-get
  '(progn
     (setq el-get-sources '(
			    browse-kill-ring
			    dired-toggle-sudo
			    fill-column-indicator
			    idomenu
			    java-mode-indent-annotations
			    magit
			    markdown-mode
			    smex
			    switch-window
			    textlint
			    undo-tree
      ))

     (when (executable-find "latex")
       (add-to-list 'el-get-sources 'auctex)
       (add-to-list 'el-get-sources 'reftex))

     (when (executable-find "sbcl")
       (add-to-list 'el-get-sources 'slime))
     
     (el-get 'sync el-get-sources)))

(load "general")
