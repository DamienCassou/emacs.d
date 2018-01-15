(require 'exwm)
(require 'exwm-systemtray)
(require 'exwm-config)

(require 'seq)
(require 'buffer-move)

(exwm-systemtray-enable)

;; Set the initial number of workspaces.
(setq exwm-workspace-number 2)
(setq exwm-workspace-show-all-buffers t)
(setq exwm-layout-show-all-buffers t)

(defun start-clipboard-manager ()
  "Start a clipboard manager, performing `kill-new' from xclip."
  (interactive)
  (start-process-shell-command "clipboard-manager"
			       nil
			       (locate-user-emacs-file "bin/clipboard-manager.sh")))

(add-hook 'exwm-init-hook #'start-clipboard-manager)

;; All buffers created in EXWM mode are named "*EXWM*". You may want to change
;; it in `exwm-update-class-hook' and `exwm-update-title-hook', which are run
;; when a new window class name or title is available. Here's some advice on
;; this subject:
;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
;; + Only renaming buffer in one hook and avoid it in the other. There's no
;;   guarantee on the order in which they are run.
;; + For applications with multiple windows (e.g. GIMP), the class names of all
;;   windows are probably the same. Using window titles for them makes more
;;   sense.
;; + Some application change its title frequently (e.g. browser, terminal).
;;   Its class name may be more suitable for such case.
;; In the following example, we use class names for all windows expect for
;; Java applications and GIMP.
(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

;; `exwm-input-set-key' allows you to set a global key binding (available in
;; any case). Following are a few examples.
;; + We always need a way to go back to line-mode from char-mode
(exwm-input-set-key (kbd "s-r") #'exwm-reset)
;; + Bind a key to switch workspace interactively
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
;; + Bind "s-0" to "s-9" to switch to the corresponding workspace.
(dotimes (i 10)
  (exwm-input-set-key (kbd (format "s-%d" i))
                      `(lambda ()
                         (interactive)
                         (exwm-workspace-switch-create ,i))))
;; + Application launcher ('M-&' also works if the output buffer does not
;;   bother you). Note that there is no need for processes to be created by
;;   Emacs.
(exwm-input-set-key (kbd "s-&")
                    (lambda (command)
                      (interactive (list (read-shell-command "$ ")))
                      (start-process-shell-command command nil command)))

;; The following example demonstrates how to set a key binding only available
;; in line mode. It's simply done by first push the prefix key to
;; `exwm-input-prefix-keys' and then add the key sequence to `exwm-mode-map'.
;; The example shorten 'C-c q' to 'C-q'.
(push ?\C-q exwm-input-prefix-keys)
(define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

;; The following example demonstrates how to use simulation keys to mimic the
;; behavior of Emacs. The argument to `exwm-input-set-simulation-keys' is a
;; list of cons cells (SRC . DEST), where SRC is the key sequence you press and
;; DEST is what EXWM actually sends to application. Note that SRC must be a key
;; sequence (of type vector or string), while DEST can also be a single key.
(exwm-input-set-simulation-keys
 '(
   ;; movement
   ([?\C-b] . left)
   ([?\M-b] . C-left)
   ([?\C-f] . right)
   ([?\M-f] . C-right)
   ([?\C-p] . up)
   ([?\C-n] . down)
   ([?\C-a] . home)
   ([?\C-e] . end)
   ([?\M-v] . prior)
   ([?\C-v] . next)
   ([?\C-d] . delete)
   ([?\C-k] . (S-end delete))
   ;; cut/paste, selection
   ([?\C-w] . ?\C-x)
   ([?\M-w] . ?\C-c)
   ([?\C-y] . ?\C-v)
   ([?\C-x h] . ?\C-a)
   ;; search
   ([?\C-s] . ?\C-f)
   ;; escape
   ([?\C-g] . escape)))

;; You can hide the mode-line of floating X windows by uncommenting the
;; following lines
                                        ;(add-hook 'exwm-floating-setup-hook #'exwm-layout-hide-mode-line)
                                        ;(add-hook 'exwm-floating-exit-hook #'exwm-layout-show-mode-line)

;; You can hide the minibuffer and echo area when they're not used, by
;; uncommenting the following line
                                        ;(setq exwm-workspace-minibuffer-position 'bottom)

;; Do not forget to enable EXWM. It will start by itself when things are ready.
;; (exwm-enable)

(add-hook 'exwm-init-hook #'display-battery-mode)
(add-hook 'exwm-init-hook #'display-time-mode)


;; Commands

(defun adjust-backlight (delta)
  (shell-command-to-string (format "brightnessctl s %s" delta)))

(defun brightness-get ()
  (let ((output (shell-command-to-string "brightnessctl")))
    (string-match "\\([0-9]+%\\)" output)
    (match-string 0 output)))

(defun increase-backlight ()
  (interactive)
  (adjust-backlight "10%+")
  (message "Backlight: %s" (brightness-get)))

(defun decrease-backlight ()
  (interactive)
  (adjust-backlight "10%-")
  (message "Backlight: %s" (brightness-get)))

(defun volume-get ()
  (let ((output (shell-command-to-string "amixer get Master")))
    (string-match "\\([0-9]+%\\)" output)
    (match-string 0 output)))

(defun adjust-volume (delta)
  "DELTA should be like \"5%+\"."
  (shell-command-to-string (format "amixer set Master %s" delta))
  ;; (shell-command-to-string "aplay ~/.stumpwm.d/sounds/drip.ogg")
  (message "Volume: %s" (volume-get)))

(defun increase-volume ()
  (interactive)
  (adjust-volume "5%+"))

(defun decrease-volume ()
  (interactive)
  (adjust-volume "5%-"))

(defun toggle-volume ()
  (interactive)
  (shell-command-to-string "exec amixer set Master toggle"))

(defun screenshot ()
  (interactive)
  (let ((default-directory (expand-file-name "~/Pictures")))
    (start-process-shell-command "scrot" nil "scrot")))

(defun screenshot-part ()
  (interactive)
  (let ((default-directory (expand-file-name "~/Pictures")))
    (start-process-shell-command "scrot -s" nil "scrot -s")))

(defun lock-screen ()
  (interactive)
  (shell-command-to-string "slock"))

(defun start-redshift ()
  (interactive)
  (let ((bufname (generate-new-buffer-name "*redshift output*")))
    (start-process-shell-command "redshift" bufname "redshift")))

(add-hook 'exwm-init-hook #'start-redshift)

(require 'exwm-randr)
;; (setq exwm-randr-workspace-output-plist '(0 "eDP-1" 1 "DP-2-1"))
(setq exwm-randr-workspace-output-plist '(1 "eDP-1" 2 "HDMI-2"))
(defun my/adapt-screen-to-setup ()
  "Use xrandr to configure screen setup."
  (start-process-shell-command
   "xrandr" nil "xrandr --output HDMI-2 --right-of eDP-1 --auto"))

(add-hook 'exwm-randr-screen-change-hook #'my/adapt-screen-to-setup)
(exwm-randr-enable)

(defun list-all-windows ()
  (seq-mapcat (lambda (frame)
		(window-list frame))
	      (frame-list)))

(defun nico-switch-to-window (bufname)
  (interactive (list (completing-read "Select window: "
                                      (seq-map #'buffer-name
                                               (seq-map #'window-buffer
                                                        (list-all-windows)))
                                      t)))
  (when-let ((window (seq-find (lambda (window)
                                 (string= (buffer-name (window-buffer window))
                                          bufname))
                               (list-all-windows))))
    (select-window window)))

(exwm-input-set-key (kbd "<XF86MonBrightnessUp>") #'increase-backlight)
(exwm-input-set-key (kbd "<XF86MonBrightnessDown>") #'decrease-backlight)
(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'increase-volume)
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'decrease-volume)
(exwm-input-set-key (kbd "<XF86AudioMute>") #'toggle-volume)
(exwm-input-set-key (kbd "<print>") #'screenshot)
(exwm-input-set-key (kbd "S-<print>") #'screenshot-part)
(exwm-input-set-key (kbd "s-l") #'lock-screen)
(exwm-input-set-key (kbd "C-x w") #'nico-switch-to-window)

(exwm-input-set-key (kbd "<s-up>") #'buf-move-up)
(exwm-input-set-key (kbd "<s-down>") #'buf-move-down)
(exwm-input-set-key (kbd "<s-left>") #'buf-move-left)
(exwm-input-set-key (kbd "<s-right>") #'buf-move-right)

(provide 'init-exwm)
