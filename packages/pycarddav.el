;;; pycarddav.el --- Integrate pycarddav in Emacs and mu4e
;;; Commentary:
;;
;;; Code:

(require 's)
(require 'mu4e-vars)
(require 'mu4e-utils)

(defun pycarddav-get-contacts-buffer ()
  "Put all pycarddav contacts in the returned buffer."
  (let ((buffer (get-buffer-create "*mu4e~pycarddav-contacts*")))
    (with-current-buffer buffer
      (erase-buffer)
      (call-process
       "pc_query"
       nil ;; input file
       (list buffer nil) ;; output to buffer, discard error
       nil ;; don't redisplay
       "-m") ;; 1st arg to pc_query: prints email addresses
      (goto-char (point-min))
      (kill-whole-line 1))
    buffer))

(defun pycarddav-get-contact-from-line (line)
  "Return a carddav contact read from LINE.

The line must start with something like:
some@email.com	Some Name

The returned contact is of the form
 (:name \"Some Name\" :mail \"some@email.com\")"
  (when (string-match "\\(.*?\\)\t\\(.*?\\)\t" line)
    (list :name (match-string 2 line) :mail (match-string 1 line))))

(defun pycarddav--helm-source-init ()
  (helm-candidate-buffer (pycarddav-get-contacts-buffer)))

(defun pycarddav--helm-source-select-action (candidate)
  (loop for candidate in (helm-marked-candidates)
        do (let ((contact (pycarddav-get-contact-from-line candidate)))
             (insert (format "\"%s\" <%s>, "
                             (plist-get contact :name)
                             (plist-get contact :mail))))))

(defclass pycarddav--helm-source (helm-source-in-buffer)
  ((init :initform #'pycarddav--helm-source-init)
   (nohighlight :initform t)
   (action :initform (helm-make-actions
                      "Select" #'pycarddav--helm-source-select-action))
   (requires-pattern :initform 0)))

(defun pycarddav-search-with-helm ()
  (interactive)
  (helm
   :prompt "contacts: "
   :sources (helm-make-source "Contacts" 'pycarddav--helm-source)))

(define-key message-mode-map (kbd "M-/") #'pycarddav-search-with-helm)
(provide 'pycarddav)

;;; pycarddav.el ends here
