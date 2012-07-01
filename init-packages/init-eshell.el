(setq eshell-highlight-prompt nil)

(defun my:eshell-prompt ()
  (let ((pwd (abbreviate-file-name (eshell/pwd))))
    (add-text-properties 0 (length pwd)
			 '(face highlight) pwd)
    (concat
     pwd
     (if
	 (=
	  (user-uid)
	  0)
	 " # " " > "))))

(setq eshell-prompt-function 'my:eshell-prompt)
