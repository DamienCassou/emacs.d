(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(require 'bibtex-utils)
(load "mybibtex" t t t)
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
		'("Bibtex all" "multibib/bibtexall" TeX-run-BibTeX
		  nil t :help "Run Bibtex on all aux files") t))
