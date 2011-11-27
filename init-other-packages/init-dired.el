; Configuring dired

(eval-after-load "dired-aux"
  '(add-to-list
    'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip")))

(require 'dired)
(load "dired-x")

(add-to-list 'completion-ignored-extensions ".log")
(add-to-list 'dired-omit-extensions ".log")
(add-to-list 'completion-ignored-extensions ".out")
(add-to-list 'dired-omit-extensions ".out")

(add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))

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
