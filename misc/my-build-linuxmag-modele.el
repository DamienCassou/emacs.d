(defun my/export-article ()
  "Convert ox-linuxmag's model-article.org into an ODT file and
compare that with ./content.xml."
  (interactive)
  (let* ((source-file "/home/cassou/.emacs.d/lib/ox-linuxmag/modele-article.org")
         (comparison-file "/home/cassou/.emacs.d/misc/content.xml")
         (zip-content (with-current-buffer
                          (find-file-noselect source-file)
                        (org-export-as 'linuxmag)))
         (comparison-buffer (find-file-noselect comparison-file)))
    (switch-to-buffer (get-buffer-create "*my/export-article*"))
    (erase-buffer)
    (nxml-mode)
    (insert zip-content)
    (let ((shell-command-dont-erase-buffer nil))
      (shell-command-on-region
       (point-min) (point-max)
       "xmllint --format -"
       (current-buffer) t))
    (require 'ediff)
    (ediff-regions-internal
     comparison-buffer (with-current-buffer comparison-buffer (point-min)) (with-current-buffer comparison-buffer (point-max))
     (current-buffer) (with-current-buffer (current-buffer) (point-min)) (with-current-buffer (current-buffer) (point-max))
     nil 'ediff-regions-wordwise 'word-mode nil)))
