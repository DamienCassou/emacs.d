(defvar er/temporary-bindings
  '(("=" . (er/expand-region 1))
    ("-" . (er/contract-region 1))))

(defun er/prepare-for-futher-expand ()
  "Let one expand more by just pressing the last key.
- < C-x = = > will expand two times
- < C-x = = - > will expand two times and then undo one"
  (let* ((repeat-key (event-basic-type last-input-event))
	 (message
	  (format "Press %s to continue expanding, - to contract"
		  (format-kbd-macro (vector repeat-key)))))
    (set-temporary-overlay-map
     (let ((map (make-sparse-keymap)))
       (dolist (binding er/temporary-bindings map)
	 (define-key map (kbd (car binding))
	   `(lambda ()
	      (interactive)
	      (eval ,(cdr binding))
	      (message ,message)))))
     t)
    (message message)))

(global-set-key (kbd "C-x =")
		(lambda ()
		  (interactive)
		  (call-interactively 'er/expand-region)
		  (er/prepare-for-futher-expand)))
