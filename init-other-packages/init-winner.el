(defun winner:prepare-for-futher-undo ()
  "Let one undo more by just pressing the last key.
Using standard configuration and this function, the user will be
able to type <C-c left left left> to undo 3 times whereas it was
<C-c left C-c left C-c left> before."
  (let* ((repeat-key (event-basic-type last-input-event))
	 (repeat-key-str (format-kbd-macro (vector repeat-key)))
	 (message (format "Type %s again to continue switching"
			  (format-kbd-macro (vector repeat-key)))))
    (set-temporary-overlay-map
     (let ((map (make-sparse-keymap)))
       (define-key map (vector repeat-key)
	 `(lambda () (interactive)
	    (winner-undo)
	    (message ,message)))
       map) t)
    (message message)))

(define-key winner-mode-map (kbd "C-c <left>")
  (lambda ()
    (interactive)
    (winner-undo)
    (winner:prepare-for-futher-undo)))
