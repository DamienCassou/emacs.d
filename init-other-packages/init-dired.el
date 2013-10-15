;;; Configuring dired

(require 'dired-aux)
(require 'dired-x)

(defun dired-do-ispell (&optional arg)
  "Mark files in dired before running this function and they will
all get spell checked."
  (interactive "P")
  (dolist (file (dired-get-marked-files
                 nil arg
                 #'(lambda (f)
                     (not (file-directory-p f)))))
    (save-window-excursion
      (with-current-buffer (find-file file)
        (ispell-buffer)))
    (message nil)))

(eval-after-load "recentf"
  '(progn
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
                    (not (tramp-tramp-file-p cur-dir-no-slash)))
           ;; recentf does not play well with file ending with a slash
           (recentf-add-file cur-dir-no-slash))))
     (add-hook 'dired-mode-hook 'recentf-track-dired-buffers t)))

(defun dc:command-has-output-p (command)
  "Returns t if COMMAND will print meaningful information."
  (not (string= "nohup " (substring command 0 6))))

;; Redefines this function from dired-aux to
;; - have the COMMAND in the output buffer name
;; - avoid opening a window when the command is not going to produce
;;   any output
(defun dired-run-shell-command (command)
  (let ((output-buffer (generate-new-buffer-name
                        (concat "*Shell Command Output: '" command "'*")))
        (handler
         (find-file-name-handler (directory-file-name default-directory)
                                 'shell-command)))
    (if handler
        (apply handler 'shell-command (list command))
      (if (dc:command-has-output-p command)
          (shell-command command output-buffer)
        (save-window-excursion
          (shell-command command output-buffer)))))
  nil)

(add-to-list
 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip"))

(let ((extensions-to-ignore '(".out" ".lol" ".ali")))
  (mapcar (lambda (extension)
            (add-to-list 'completion-ignored-extensions extension)
            (add-to-list 'dired-omit-extensions extension))
          extensions-to-ignore))

(let ((files-to-ignore '("Thumbs\.db" "Thumbs\.db:encryptable" "b~.*\.ad[sb]")))
  (mapcar (lambda (filename)
            (setq dired-omit-files
                  (concat dired-omit-files "\\|^" filename "$")))
          files-to-ignore))

(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))

;; Auto-rotate pictures according to EXIF data
(eval-after-load "image-dired"
  '(progn
     (setq image-dired-cmd-create-thumbnail-options
           (replace-regexp-in-string "-strip" "-auto-orient -strip" image-dired-cmd-create-thumbnail-options)
           image-dired-cmd-create-temp-image-options
           (replace-regexp-in-string "-strip" "-auto-orient -strip" image-dired-cmd-create-temp-image-options))))

(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line (if dired-omit-mode 2 4)))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
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
  (vector 'remap 'move-beginning-of-line) 'dired-move-beginning-of-line)

(let ((dired-guessing
       '(
         ("\\.avi" "vlc")
         ("\\.docx?" "libreoffice")
         ("\\.flv" "vlc")
         ("\\.html?" "firefox")
         ("\\.image" "pharo-vm-x")
         ("\\.jar" "java -jar")
         ("\\.jpg" "eog")
         ("\\.mkv" "vlc")
         ("\\.mp4" "vlc")
         ("\\.od[pts]" "libreoffice")
         ("\\.ogv" "vlc")
         ("\\.pdf" "evince")
         ("\\.png" "eog")
         ("\\.pptx?" "libreoffice")
         ("\\.sh" "bash")
         ("\\.svg" "inkscape")
         ("\\.uxf" "~/Downloads/Umlet/umlet.sh")
         ("\\.webm" "vlc")
         ("\\.xcf" "gimp")
         ("\\.xlsx?" "libreoffice")
         ("\\.xslx?" "libreoffice")
         )))
  (mapcar (lambda (pair)
            (add-to-list 'dired-guess-shell-alist-user
                         (list (first pair) (concat "nohup " (second pair)))))
          dired-guessing))
