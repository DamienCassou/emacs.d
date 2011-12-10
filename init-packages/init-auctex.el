(load "mybibtex" t t t)
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
		'("Bibtex all" "multibib/bibtexall" TeX-run-BibTeX
		  nil t :help "Run Bibtex on all aux files") t))

(eval-after-load "latex"
  `(defun LaTeX-align-table ()
     (interactive)
     (save-excursion
       (LaTeX-mark-environment)
       (while (re-search-forward "& *" (region-end) t)
	 (replace-match "& " nil nil))
       (LaTeX-mark-environment)
       (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)\\(&\\|\\\\\\\\\\)" 1 1 t))))
