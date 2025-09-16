;;; early-init.el -*- lexical-binding: t; -*-

;; My Nix configuration seems to mess up with variable
;; `invocation-directory': instead of referencing the package with
;; emacs and its dependencies, it is referencing the package
;; containing only Emacs. This is problematic for some tools such as
;; `elisp-flymake-byte-compile' and `borg--build-interactive' which
;; don't find necessary libraries.
(when (string-match-p "/nix/store.*-emacs-gtk" invocation-directory)
  (setq invocation-directory
        (let ((emacs-bin-store-path (file-chase-links (expand-file-name invocation-name "~/.nix-profile/bin"))))
          (file-name-parent-directory emacs-bin-store-path))))

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'.
(setq package-enable-at-startup nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(setq tool-bar-mode nil)
(setq menu-bar-mode nil)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

(when (eq system-type 'darwin)
  (dolist (path '("/run/current-system/sw/bin" "/etc/profiles/per-user/cassou/bin" "/usr/local/bin" "/opt/homebrew/bin" ))
    (add-to-list 'exec-path path)
    (setenv "PATH" (concat path ":" (getenv "PATH"))))

  (setenv "SSH_AUTH_SOCK" (expand-file-name "~/.gnupg/S.gpg-agent.ssh"))

  ;; Enforce asking permissions immediately:
  (shell-command "ls ~/Documents")
  (shell-command "ls ~/Desktop")
  (shell-command "ls ~/Downloads")
  (shell-command "ls ~/Movies/TV")
  (shell-command "ls ~/Pictures/'Photo Booth Library'")
  (shell-command "ping -c 1 raspberrypi.local"))
