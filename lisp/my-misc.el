;;; my-misc.el --- misc stuff                           -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Misc stuff

;;; Code:

(defun set-selected-frame-dark ()
  "Make current frame use GTK dark theme."
  (interactive)
  (let ((frame-name (cdr (assq 'name (frame-parameters (selected-frame))))))
    (call-process-shell-command
     (format
      "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT 'dark' -name '%s'"
      frame-name))))

(add-to-list 'custom-theme-load-path "~/.emacs.d/packages/zerodark-theme")

(defun my:setup-frame ()
  "Configure look of current frame."
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (when (window-system)
    (ignore-errors
      (load-theme 'zerodark)
      (zerodark-setup-modeline-format))
    (set-selected-frame-dark)
    (set-face-attribute 'default nil :height 125 :family "Fira Mono")))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (my:setup-frame)))
  (my:setup-frame))

(defun suspend-on-tty-only ()
  "Suspend Emacs, but only if in tty mode."
  (interactive)
  (unless window-system
    (suspend-frame)))

(bind-key "C-x C-z" 'suspend-on-tty-only)

(add-to-list 'default-frame-alist '(cursor-type bar . 3))

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

(bind-key "<f5>" 'comment-region)

;; Replace `just-one-space' by the more advanced `cycle-spacing'.
(bind-key "M-SPC" #'cycle-spacing)

(defun my-join-line ()
  (interactive)
  (join-line -1))

(bind-key "M-j" 'my-join-line)

;; Use kill-buffer instead of kill-this-buffer because the latter's
;; docstring says it is unreliable
(bind-key "C-x k" (lambda () (interactive) (kill-buffer (current-buffer))))

(defun toggle-window-split ()
  "Swap between horizontal and vertical separation when 2 frames
are visible."
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

(define-prefix-command 'endless/toggle-map)
(setq endless/toggle-prefix "C-. t")
(bind-key endless/toggle-prefix 'endless/toggle-map)

(bind-key "d" 'toggle-debug-on-error endless/toggle-map)

(bind-key "C-x 8 <S-right>" (lambda () (interactive) (insert-char ?→))) ; rightwards arrow
(bind-key "C-x 8 <right>" (lambda () (interactive) (insert-char ?⇒))) ; rightwards double arrow

(bind-key "C-x 8 <S-left>" (lambda () (interactive) (insert-char ?←))) ; leftwards arrow
(bind-key "C-x 8 <left>" (lambda () (interactive) (insert-char ?⇐))) ; leftwards double arrow

(bind-key "C-x 8 <S-up>" (lambda () (interactive) (insert-char ?↑))) ; upwards arrow
(bind-key "C-x 8 <up>" (lambda () (interactive) (insert-char ?⇑))) ; upwards double arrow

(bind-key "C-x 8 <S-down>" (lambda () (interactive) (insert-char ?↓))) ; downwards arrow
(bind-key "C-x 8 <down>" (lambda () (interactive) (insert-char ?⇓))) ; rightwards double arrow

(bind-key "<S-left>" #'beginning-of-buffer)
(bind-key "<S-right>" #'end-of-buffer)
(unbind-key "M-<")
(unbind-key "M->")

(defun my/force-reload-files ()
  "Force reload all elisp files in the current directory."
  (interactive)
  (seq-do #'load-file
          (seq-filter (lambda (filename)
                        (string-suffix-p ".el" filename))
                      (directory-files "."))))

(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

(defun my/mount-backup-disk (&optional unmount)
  "Mount backup disk.  Unmount if UNMOUNT is t.
Interactively, unmount when prefix argument."
  (interactive "P")
  (let ((script (expand-file-name "~/Documents/configuration/scripts/lacie-mount.sh")))
    (require 'em-term)
    (if unmount
        (eshell-exec-visual script "-1")
      (eshell-exec-visual script))))

(defun my/unmount-backup-disk ()
  "Unmount backup disk."
  (interactive)
  (my/mount-backup-disk t))

(put 'narrow-to-region 'disabled nil)

;; https://oremacs.com/2016/02/24/dired-rsync/
;;;###autoload
(defun my/dired-rsync (dest)
  (interactive
   (list
    (expand-file-name
     (read-file-name
      "Rsync to:"
      (dired-dwim-target-directory)))))
  ;; store all selected files into "files" list
  (let ((files (dired-get-marked-files
                nil current-prefix-arg))
        ;; the rsync command
        (tmtxt/rsync-command
         "rsync -arvz --progress "))
    ;; add all selected file names as arguments
    ;; to the rsync command
    (dolist (file files)
      (setq tmtxt/rsync-command
            (concat tmtxt/rsync-command
                    (shell-quote-argument file)
                    " ")))
    ;; append the destination
    (setq tmtxt/rsync-command
          (concat tmtxt/rsync-command
                  (shell-quote-argument dest)))
    ;; run the async shell command
    (async-shell-command tmtxt/rsync-command "*rsync*")
    ;; finally, switch to that window
    (other-window 1)))

(define-key dired-mode-map "Y" 'ora-dired-rsync)

(defun my/youtube-dl ()
  (interactive)
  (let* ((str (current-kill 0))
         (default-directory "/tmp")
         (proc (get-buffer-process (ansi-term "/bin/bash"))))
    (term-send-string
     proc
     (concat "cd /tmp && youtube-dl " str "\n"))))

(defun my/swap-last-buffers ()
  "Replace currently visible buffer by last one."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))

(bind-key "C-x B" #'my/swap-last-buffers)

(define-minor-mode screencast-mode
  "Activate/deactivate some modes to facilitate screencasting."
  :lighter ""
  :global t
  (if screencast-mode
      (progn
        (beacon-mode -1)
        (blink-cursor-mode -1)
        (global-visible-mark-mode -1))))

(provide 'my-misc)
;;; my-misc.el ends here

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
