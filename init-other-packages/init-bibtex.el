(add-to-list 'bibtex-BibTeX-entry-alist
	     '("software" "A software"
	       (("title")
		("version")
		("organization")
		("month")
		("year")
		("id"))
	       nil
	       nil))

;; "keywords" should be a recognized field
(mapc
 (lambda (list)
   (push '("keywords") (fifth list)))
 bibtex-BibTeX-entry-alist)
