(defun stef:defkey (keystroke charname)
  "Bind KEYSTROKE to insert the unicode CHARNAME."
  (lexical-let ((charname charname)) ;; we want the next lambda to be
				     ;; a closure
    (global-set-key (read-kbd-macro keystroke)
		    (lambda ()
		      (interactive)
		      (ucs-insert  (cdr (assoc charname (ucs-names))))))))

;; To get the list of all unicode character names and their
;; representation, type C-x 8 RET

;; You always have to unbind the prefix of each keystroke in case it
;; is already used as a non prefix keystroke.
(global-unset-key (kbd "M-e"))
(stef:defkey "M-e e" "LATIN SMALL LETTER E ACUTE")

(global-unset-key (kbd "M-`"))
(stef:defkey "M-` e" "LATIN SMALL LETTER E GRAVE")
(stef:defkey "M-` a" "LATIN SMALL LETTER A GRAVE")
(stef:defkey "M-` u" "LATIN SMALL LETTER U GRAVE")

(stef:defkey "M-c" "LATIN SMALL LETTER C CEDILLA")

(global-unset-key (kbd "M-i"))
(stef:defkey "M-i i" "LATIN SMALL LETTER I CIRCUMFLEX")

(global-unset-key (kbd "M-o o"))
(stef:defkey "M-o o" "LATIN SMALL LETTER O CIRCUMFLEX")
