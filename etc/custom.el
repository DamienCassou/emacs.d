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
   (quote
    ("d0ef31755ffdd424b5362a138cf42a2d215cc614964b8709117df5ab8f6e7386")))
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
    ((projectile-project-type quote passwe)
     (sh-shell . "sh")
     (js2-missing-semi-one-line-override)
     (flycheck-javascript-eslint-executable . "/home/cassou/Documents/projects/ftgp/widgetjs/node_modules/.bin/eslint")
     (js2-missing-semi-one-line-override . t)
     (js2-strict-missing-semi-warning)
     (eval flycheck-cask-setup)
     (ispell-dictionary . "french")
     (eval add-to-list
           (quote grep-find-ignored-files)
           "archive-contents"))))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
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
 )

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
