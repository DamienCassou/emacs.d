(defun winner:undo-again ()
  (interactive)
  (setq this-command 'winner-undo)
  (winner-undo))

(defun winner:redo-again ()
  (interactive)
  (setq this-command 'winner-undo)
  (winner-redo))

(defun winner:prepare-for-futher-undo ()
  "Let one undo more by just pressing the last key.
Using standard configuration and this function, the user will be
able to type <C-c left left left> to undo 3 times whereas it was
<C-c left C-c left C-c left> before."
  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "<left>") 'winner:undo-again)
     (define-key map (kbd "<right>") 'winner:redo-again)
     map) t)
  (message "Type <left>/<right> to continue switching"))

(defun winner:initial-undo ()
    (interactive)
    (setq this-command 'winner-undo)
    (winner-undo)
    (winner:prepare-for-futher-undo))

(define-key winner-mode-map (kbd "C-c <left>")
  'winner:initial-undo)
