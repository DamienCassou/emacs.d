(setq inferior-lisp-program "sbcl")
(setq inferior-lisp-program "~/.emacs.d/ibcl")
(require 'slime)
(slime-setup '(slime-fancy slime-asdf slime-hyperdoc))

(define-key (eval 'slime-mode-map) (kbd "C-c c") (lambda (&optional arg)
     "Copy/past the previous sexp to the slime buffer"
     (interactive "p")
     (kmacro-exec-ring-item (quote
			     ([67108896 134217730 134217847 24 111 25 return 24 111 134217734 134217734] 0 "%d")) arg)))


(defun build-slide ()
  (interactive)
  (with-current-buffer (current-buffer)
    (save-excursion
      (re-search-backward "^(slide")
      (mark-sexp)
      (kill-ring-save (region-beginning) (region-end))
      (kill-ring-save (point-min) (point-max))
      (with-temp-file "one-slide.tmp"
	(yank)
	(insert "(deck-reset)")
	(yank 2))
      (with-temp-file "compile.tmp"
	(insert "(load \"dslides.lisp\")(load \"one-slide.tmp\")(deck-real-output)"))
      (if (file-exists-p "slides-output.tex")
	  (delete-file  "slides-output.tex" nil))
      (when (get-buffer "*dslides-output*")
	(with-current-buffer "*dslides-output*"
	  (erase-buffer)))
      (call-process (expand-file-name "~/.emacs.d/ibcl")
		    "compile.tmp" "*dslides-output*" nil)
      (let ((file-size (car (nthcdr 7 (file-attributes "slides-output.tex")))))
	(when (or (null file-size) (zerop file-size))
	  (error "slides-output.tex is empty, something went wrong with ibcl")))
      (call-process "pdflatex" nil "*dslides-output*" nil "clos.tex"))))

(defadvice message (around inhibit-message (format-string &rest args)
			   activate disable)
  (if (string-equal format-string "DocView: process %s changed status to %s.")
      (setq format-string ""))
    ad-do-it)

(defun show-slide ()
  (interactive)
  (build-slide)
  (let ((directory (file-name-directory (buffer-file-name))))
    (other-window 1)
    (ad-enable-advice 'message 'around 'inhibit-message)
    (ad-activate 'message)
    (let ((revert-without-query '("clos\.pdf")))
      (find-file (expand-file-name (concat directory "clos.pdf"))))
    (doc-view-first-page)
    (ad-disable-advice 'message 'around 'inhibit-message)
    (ad-activate 'message)
    (doc-view-fit-page-to-window)))

(define-key (eval 'slime-mode-map) (kbd "C-c v") #'show-slide)
