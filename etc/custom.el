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
   '("20f5690eb69b06a27c4c9dfcd386ca4945622d56c3469ecc38cacc183781e613" default))
 '(delete-by-moving-to-trash t)
 '(enable-recursive-minibuffers t)
 '(frame-title-format "Emacs: %b" t)
 '(gc-cons-threshold 20000000)
 '(indent-tabs-mode nil)
 '(load-prefer-newer t)
 '(menu-bar-mode nil)
 '(next-screen-context-lines 5)
 '(safe-local-variable-values
   (quote
    ((flycheck-mode . t)
     (js2-missing-semi-one-line-override)
     (org-use-sub-superscripts . {})
     (TeX-master . "cv-esthetique")
     (TeX-PDF-mode . t)
     (projectile-project-run-cmd . "mkdir -p build; cd build; cmake ..; make run")
     (projectile-project-compilation-cmd . "mkdir -p build; cd build; cmake ..; make")
     (eval when
           (and
            (buffer-file-name)
            (not
             (file-directory-p
              (buffer-file-name)))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (unless
               (featurep 'package-build)
             (let
                 ((load-path
                   (cons "../package-build" load-path)))
               (require 'package-build)))
           (unless
               (derived-mode-p 'emacs-lisp-mode)
             (emacs-lisp-mode))
           (package-build-minor-mode)
           (setq-local flycheck-checkers nil)
           (set
            (make-local-variable 'package-build-working-dir)
            (expand-file-name "../working/"))
           (set
            (make-local-variable 'package-build-archive-dir)
            (expand-file-name "../packages/"))
           (set
            (make-local-variable 'package-build-recipes-dir)
            default-directory))
     (TeX-master . "geiser")
     (org-export-initial-scope . buffer)
     (org-id-link-to-org-use-id)
     (org-export-with-broken-links . t)
     (eval require 'org-make-toc)
     (eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'")
     (ispell-dictionary . "french")
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
     (eval when
           (and
            (buffer-file-name)
            (file-regular-p
             (buffer-file-name))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (unless
               (featurep 'package-build)
             (let
                 ((load-path
                   (cons "../package-build" load-path)))
               (require 'package-build)))
           (package-build-minor-mode)
           (set
            (make-local-variable 'package-build-working-dir)
            (expand-file-name "../working/"))
           (set
            (make-local-variable 'package-build-archive-dir)
            (expand-file-name "../packages/"))
           (set
            (make-local-variable 'package-build-recipes-dir)
            default-directory))
     (eval add-hook 'before-save-hook #'time-stamp nil t)
     (eval add-hook 'before-save-hook #'time-stamp-target nil t)
     (electric-quote-mode . t)
     (org-src-preserve-indentation)
     (firestarter let
                  ((compilation-read-command nil))
                  (projectile-test-project nil))
     (firestarter . "npm test")
     (firestarter . "gulp css")
     (org-ascii-text-width . 200000000)
     (js2-strict-missing-semi-warning)
     (eval flycheck-cask-setup))))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(transmission-rpc-auth '(:username "transmission"))
 '(truncate-partial-width-windows nil)
 '(undo-limit 5000000)
 '(undo-outer-limit 200000000)
 '(undo-strong-limit 10000000)
 '(user-full-name "Damien Cassou")
 '(visible-bell nil))

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
