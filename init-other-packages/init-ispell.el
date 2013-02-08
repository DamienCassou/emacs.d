;; ispell must ignore LaTeX commands and environments
(setq ispell-tex-skip-alists
      (list
       (append (car ispell-tex-skip-alists)
               '(("\\\\cite"            ispell-tex-arg-end)
                 ("\\\\nocite"          ispell-tex-arg-end)
                 ("\\\\includegraphics" ispell-tex-arg-end)
                 ("\\\\figScale"         ispell-tex-arg-end)
                 ("\\\\author"          ispell-tex-arg-end)
                 ("\\\\ref"             ispell-tex-arg-end)
                 ("\\\\eqref"             ispell-tex-arg-end)
                 ("\\\\pageref"             ispell-tex-arg-end)
                 ("\\\\label"           ispell-tex-arg-end)
		 ("\\\\lstinputlisting" ispell-tex-arg-end)
		 ("\\\\enword" ispell-tex-arg-end)
		 ("\\\\ct" ispell-tex-arg-end)
		 ("\\\\sigle" ispell-tex-arg-end)
		 ("\\\\nocheck" ispell-tex-arg-end)
		 ("\\\\mathit" ispell-tex-arg-end)
		 ("\\\\url" ispell-tex-arg-end)
		 ("\\\\lst[p]?\\(lignesa\\|lignes\\|ligne\\)" ispell-tex-arg-end 2)
		 ("\\\\\\(lignesa\\|lignes\\|ligne\\)" ispell-tex-arg-end)
		 ("\\\\c?cauthor" ispell-tex-arg-end)
		 ("\\\\page" ispell-tex-arg-end)
		 ("\\\\listing" ispell-tex-arg-end)
		 ("\\\\macitationraw" ispell-tex-arg-end 2)
		 ("\\\\macitation" ispell-tex-arg-end 3)
		 ("\\\\mm" ispell-tex-arg-end)
		 ("\\\\begin{lstlisting}" . "\\\\end{lstlisting}")
		 ("\\\\begin{code}{}" . "\\\\end{code}")
                 ))
       (append (cadr ispell-tex-skip-alists)
	       '(("tabular" ispell-tex-arg-end)
		 ("equation\\*" . "\\\\end[ 	\n]*{[ 	\n]*equation\\*[ 	\n]*}")
		 ("tikzpicture" . "\\\\end[ 	\n]*{[ 	\n]*tikzpicture[ 	\n]*}")))))

(require 'flyspell)
(add-hook 'text-mode-hook 'flyspell-mode)

(defun ispell-set-dictionary (dict)
  (save-excursion
    (add-file-local-variable 'ispell-local-dictionary dict)))

(defun ispell-change-dictionary-to-french ()
  (interactive)
  (ispell-change-dictionary "francais")
  (ispell-set-dictionary "francais")
  (flyspell-buffer))

(defun ispell-change-dictionary-to-english ()
  (interactive)
  (ispell-change-dictionary "english")
  (ispell-set-dictionary "english")
  (flyspell-buffer))

(defun flyspell-toggle ()
  (interactive)
  (let ((mode-value (if flyspell-mode -1 1)))
    (save-excursion
      (add-file-local-variable 'eval `(flyspell-mode ,mode-value)))
    (flyspell-mode mode-value)))

(global-set-key (kbd "s-i b") 'ispell-buffer)
(global-set-key (kbd "s-i w") 'ispell-word)
(global-set-key (kbd "s-i d f") 'ispell-change-dictionary-to-french)
(global-set-key (kbd "s-i d e") 'ispell-change-dictionary-to-english)
(global-set-key (kbd "s-i d ?") 'ispell-change-dictionary)
(global-set-key (kbd "s-f b") 'flyspell-buffer)
(global-set-key (kbd "s-f m") 'flyspell-toggle)

;; Avoids flyspell from overriding <C-.>
(setq flyspell-mode-map
      (delq (assoc 67108910 flyspell-mode-map) flyspell-mode-map))
