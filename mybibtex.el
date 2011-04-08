(defun my-bibtex-compare-key ()
  (interactive)
  (let ((current-key (downcase (save-excursion
		       (re-search-forward (if (eq entry-type 'string)
					      bibtex-string-maybe-empty-head
					    bibtex-entry-maybe-empty-head))
		       (if (match-beginning bibtex-key-in-head)
			   (buffer-substring (match-beginning bibtex-key-in-head)
					  (match-end bibtex-key-in-head))))))
	(autokey (downcase (bibtex-generate-autokey))))
    (unless (my-string-startwith-p current-key autokey)
      (display-warning '(bibtex) (concat "The generated key '" autokey
		     "' does not look like your key '"
		     current-key "' with a suffix")))
    (if (string= current-key autokey)
	(display-warning '(bibtex) (concat "There is no suffix at the end of key '" current-key "'")))))

(defun my-string-startwith-p (big-string small-string)
  (eq 0 (string-match small-string big-string)))

(assert (my-string-startwith-p "duca01a" "duca01"))
(assert (not (my-string-startwith-p "duca01a" "duca02")))
(assert (not (my-string-startwith-p "duca01a" "uca01")))
(assert (not (my-string-startwith-p "duc01" "duca01")))

(add-hook 'bibtex-clean-entry-hook 'my-bibtex-compare-key)
