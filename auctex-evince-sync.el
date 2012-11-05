;; Forward/inverse search with evince using D-bus.
(if (require 'dbus "dbus" t)
    (progn
      ;; universal time, need by evince
      (defun utime ()
        (let ((high (nth 0 (current-time)))
              (low (nth 1 (current-time))))
          (+ (* high (lsh 1 16) ) low)))

      ;; Forward search.
      ;; Adapted from http://dud.inf.tu-dresden.de/~ben/evince_synctex.tar.gz
      (defun auctex-evince-forward-sync (pdffile texfile line)
        (let ((dbus-name
	       (dbus-call-method :session
				 "org.gnome.evince.Daemon"  ; service
				 "/org/gnome/evince/Daemon" ; path
				 "org.gnome.evince.Daemon"  ; interface
				 "FindDocument"
				 (concat "file://" pdffile)
				 t     ; Open a new window if the file is not opened.
				 )))
          (dbus-call-method :session
			    dbus-name
			    "/org/gnome/evince/Window/0"
			    "org.gnome.evince.Window"
			    "SyncView"
			    texfile
			    (list :struct :int32 line :int32 1)
			    (utime))))

      (defun auctex-evince-view ()
        (let ((pdf (file-truename (concat default-directory
					  (TeX-master-file (TeX-output-extension)))))
	      (tex (buffer-file-name))
	      (line (line-number-at-pos)))
          (auctex-evince-forward-sync pdf tex line)))

      ;; New view entry: Evince via D-bus.
      (eval-after-load 'tex
	'(progn
	   (add-to-list 'TeX-view-program-list '("EvinceDbus" auctex-evince-view))

	   ;; Use Evince via D-bus for reading pdf files
	   (setcar (member-if (lambda (pair) (equal 'output-pdf (car pair)))
			      TeX-view-program-selection)
		   '(output-pdf "EvinceDbus"))))

      ;; Inverse search.
      ;; Adapted from: http://www.mail-archive.com/auctex@gnu.org/msg04175.html
      (defun auctex-evince-inverse-sync (file linecol timestamp)
        (let ((buf (get-file-buffer (substring file 7)))
              (line (car linecol))
              (col (cadr linecol)))
          (if (null buf)
              (message "Sorry, %s is not opened..." file)
            (switch-to-buffer buf)
            (goto-line (car linecol))
            (unless (= col -1)
              (move-to-column col))
	    (raise-frame))))  ;; pop the emacs window

      (dbus-register-signal
       :session nil "/org/gnome/evince/Window/0"
       "org.gnome.evince.Window" "SyncSource"
       'auctex-evince-inverse-sync)))

(provide 'auctex-evince-sync)
