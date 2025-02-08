;;; custom.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bookmark-save-flag 1)
 '(custom-safe-themes
   '("4cc1cc7efd5c2362ef684657eec7d7e482223b1def4edeb0fab52ba1d334d38a"
     "4a288765be220b99defaaeb4c915ed783a9916e3e08f33278bf5ff56e49cbc73"
     default))
 '(delete-by-moving-to-trash t)
 '(enable-recursive-minibuffers t)
 '(frame-title-format "Emacs: %b" t)
 '(gc-cons-threshold 20000000)
 '(indent-tabs-mode nil)
 '(load-prefer-newer t)
 '(next-screen-context-lines 5)
 '(org-safe-remote-resources
   '("\\`https://fniesse\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'"))
 '(related-files-jumpers
   '((recipe :remove-suffix ".el" :add-suffix "-tests.el" :add-directory
             "test")
     (recipe :remove-suffix ".el" :add-suffix "-test.el"
             :add-directory "tests")))
 '(safe-local-variable-values
   '((eval add-hook 'before-save-hook #'whitespace-cleanup nil t)
     (package-lint-main-file . "test-cockpit.el")
     (eval setq-local elisp-flymake-byte-compile-load-path
           (cons "./"
                 (let
                     ((regexp
                       (rx
                        (or "/dash" "/elcouch" "/forge/lisp"
                            "/json-mode" "/lib/markdown-mode"
                            "/libbcel" "/magit/lisp"
                            "/with-editor/lisp" "/yaml"
                            "/json-snatcher" "/navigel" "/libelcouch"
                            "/prodigy" (and "/s" string-end)
                            (and "/f" string-end) "/flycheck"
                            "/yasnippet" "/company" "/eslintd-fix"
                            "/js2-mode" "/js2-refactor"
                            "/multiple-cursors" "/tern" "/xref-js2"
                            "/ghub" "/treepy" "/reformatter"))))
                   (seq-filter
                    (lambda (path) (string-match-p regexp path))
                    load-path))))
     (org-imenu-depth . 3) (eval my/eglot-ensure)
     (finsit-insert-commit-id . t) (adaptive-wrap-extra-indent . 2)
     (org-babel-default-header-args:gitconfig (:eval . "no"))
     (org-babel-default-header-args:text (:eval . "no"))
     (org-babel-default-header-args:sh (:exports . "both")
                                       (:results . "output raw")
                                       (:session . "repo-test")
                                       (:tangle . "yes")
                                       (:wrap . "src text"))
     (org-export-global-macros
      ("fedora-version" lambda (&rest _)
       (save-excursion
         (with-temp-buffer
           (insert-file "/etc/fedora-release") (goto-char (point-min))
           (re-search-forward (rx (group-n 1 (+ digit))))
           (match-string 1))))
      ("git-version" lambda (&rest _) (magit-git-version)))
     (eval defun my-insert-shell-prompt (_backend)
           "https://emacs.stackexchange.com/questions/44958/can-i-insert-a-prefix-to-org-babel-source-code-lines-on-export/44970#44970"
           (org-babel-map-src-blocks nil
             (let
                 ((lang lang) (beg-body beg-body)
                  (end-body (copy-marker end-body)) (prefix "$ ")
                  (is-contd-from-prev-line nil))
               (when (member lang org-babel-shell-names)
                 (goto-char beg-body)
                 (skip-chars-forward "\12 " (marker-position end-body))
                 (while (< (point) (marker-position end-body))
                   (if (not is-contd-from-prev-line) (insert prefix))
                   (end-of-line)
                   (if (eq 92 (char-after (- (point) 1)))
                       (setq is-contd-from-prev-line t)
                     (setq is-contd-from-prev-line nil))
                   (skip-chars-forward "\12 "
                                       (marker-position end-body)))))))
     (eval require 'ol-man nil t) (eval require 'magit-base nil t)
     (magit-refresh-verbose . t) (org-imenu-depth . 1)
     (etags-regen-ignores "test/manual/etags/")
     (etags-regen-regexp-alist
      (("c" "objc") "/[ \11]*DEFVAR_[A-Z_ \11(]+\"\\([^\"]+\\)\"/\\1/"
       "/[ \11]*DEFVAR_[A-Z_ \11(]+\"[^\"]+\",[ \11]\\([A-Za-z0-9_]+\\)/\\1/"))
     (eval add-hook 'before-save-hook #'ledger-mode-clean-buffer nil t)
     (my/eglot-autoformat-on-save) (eval eglot-ensure)
     (magit-todos-exclude-globs "Makefile" "makem.sh")
     (completion-ignore-case . t)
     (eval add-to-list 'org-export-before-parsing-functions
           #'my-insert-shell-prompt)
     (jumprel-jumpers
      (filename :remove-suffix ".el" :add-suffix "-tests.el"
                :add-directory "test"))
     (eval flymake-hledger-enable) (vc-prepare-patches-separately)
     (eval and buffer-file-name
           (not (eq major-mode 'package-recipe-mode))
           (or (require 'package-recipe-mode nil t)
               (let ((load-path (cons "../package-build" load-path)))
                 (require 'package-recipe-mode nil t)))
           (package-recipe-mode))
     (sh-shell . "bash") (org-confirm-babel-evaluate)
     (graphviz-dot-indent-width . 2)
     (related-files-jumpers
      (recipe :remove-suffix ".js" :add-suffix "-tests.js"
              :add-directory "tests" :case-transformer uncapitalize)
      (recipe :remove-suffix ".js" :add-suffix "-tests.js"
              :add-directory "tests")
      (recipe :remove-suffix ".js" :add-suffix ".spec.component.js"
              :filler (yasnippet :name "componentSpec"))
      (recipe :remove-suffix ".js" :add-suffix ".less")
      (recipe :remove-suffix ".js" :add-suffix ".stories.js" :filler
              (yasnippet :name "stories")))
     (forge-display-in-status-buffer)
     (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w") (forge-buffer-draft-p . t)
     (js2-strict-inconsistent-return-warning)
     (eval flymake-eslint-enable)
     (flymake-eslint-executable-name . "eslint_d")
     (elisp-lint-indent-specs (describe . 1) (it . 1)
                              (org-element-map . defun)
                              (org-roam-with-temp-buffer . 1)
                              (org-with-point-at . 1)
                              (magit-insert-section . defun)
                              (magit-section-case . 0) (->> . 1)
                              (org-roam-with-file . 2))
     (elisp-lint-ignored-validators "byte-compile" "package-lint")
     (flycheck-disabled-checkers ledger)
     (flycheck-disabled-checkers quote (emacs-lisp-checkdoc))
     (project-vc-merge-submodules) (flycheck-mode . t)
     (js2-missing-semi-one-line-override)
     (org-use-sub-superscripts . {}) (TeX-master . "cv-esthetique")
     (TeX-PDF-mode . t)
     (projectile-project-run-cmd
      . "mkdir -p build; cd build; cmake ..; make run")
     (projectile-project-compilation-cmd
      . "mkdir -p build; cd build; cmake ..; make")
     (TeX-master . "geiser") (org-export-initial-scope . buffer)
     (org-id-link-to-org-use-id) (org-export-with-broken-links . t)
     (eval require 'org-make-toc) (eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'") (ispell-dictionary . "french")
     (firestarter . "yarn run less")
     (ledger-post-amount-alignment-column . 56)
     (checkdoc-package-keywords-flag)
     (ledger-post-amount-alignment-column . 52)
     (ledger-post-amount-alignment-column . 61)
     (ledger-post-amount-alignment-column . 20)
     (ledger-post-amount-alignment-column . 42)
     (ledger-post-account-alignment-column . 2)
     (flycheck-ledger-pedantic . t)
     (git-commit-major-mode . git-commit-elisp-text-mode)
     (eval add-hook 'before-save-hook #'time-stamp nil t)
     (eval add-hook 'before-save-hook #'time-stamp-target nil t)
     (electric-quote-mode . t) (org-src-preserve-indentation)
     (firestarter let ((compilation-read-command nil))
                  (projectile-test-project nil))
     (firestarter . "npm test") (firestarter . "gulp css")
     (org-ascii-text-width . 200000000)
     (js2-strict-missing-semi-warning) (eval flycheck-cask-setup)))
 '(transmission-rpc-auth '(:username "transmission"))
 '(truncate-partial-width-windows nil)
 '(undo-limit 5000000)
 '(undo-outer-limit 200000000)
 '(undo-strong-limit 10000000)
 '(user-full-name "Damien Cassou")
 '(visible-bell nil)
 '(warning-suppress-log-types '((browse-url))))

;;; Emacs Configuration
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(widgetjs-html-tag-face ((t (:foreground "#61afef" :box 1 :weight bold)))))

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
