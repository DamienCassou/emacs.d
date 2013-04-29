(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(require 'magit-svn)
(add-hook 'magit-mode-hook 'turn-on-magit-svn)

;; Use shortcuts of drag-stuff mode to move lines up/down in rebase-mode
(eval-after-load "drag-stuff"
  '(eval-after-load "rebase-mode"
     '(progn
        (define-key rebase-mode-map (drag-stuff--kbd 'up) 'rebase-mode-move-line-up)
        (define-key rebase-mode-map (drag-stuff--kbd 'down) 'rebase-mode-move-line-down))))

;; Make C-x C-j the same as in all other modes
(define-key magit-mode-map (kbd "C-x C-j")
  '(lambda ()
     (interactive)
     (dired default-directory)))

(defun magit-ignore-latex-project ()
  (interactive)
  (mapcar #'magit-ignore-file (list "*.aux" "*.log" "*.out" "*.bbl" "*.blg" "auto/" "*.synctex.gz"))
  (magit-refresh))
