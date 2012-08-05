(defun winner:prepare-for-futher-undo ()
  "Let one undo more by just pressing the last key.
Using standard configuration and this function, the user will be
able to type <C-c left left left> to undo 3 times whereas it was
<C-c left C-c left C-c left> before."
  (let* ((message (format "Type <left>/<right> to continue switching")))
    (set-temporary-overlay-map
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "<left>")
	 `(lambda () (interactive)
	    (setq this-command 'winner-undo)
	    (winner-undo)))
       (define-key map (kbd "<right>")
	 `(lambda () (interactive)
	    (setq this-command 'winner-undo)
	    (winner-redo)))
       map) t)
    (message message)))

(define-key winner-mode-map (kbd "C-c <left>")
  (lambda ()
    (interactive)
    (setq this-command 'winner-undo)
    (winner-undo)
    (winner:prepare-for-futher-undo)))
