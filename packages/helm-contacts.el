;;; http://kitchingroup.cheme.cmu.edu/blog/2015/03/14/A-helm-mu4e-contact-selector/

;; here we set aliases for groups.
(setq email-groups
      '(("ms" . "email1, email2")
        ("phd" . "email3, email4")))

(defun org-contacts-open-from-email (email)
  "Open org-contact with matching EMAIL. If no match, create new
entry with prompts for first and last name."
  (let ((contact (catch 'contact
                   (loop for contact in  (org-contacts-db)
                         do
                         (when (string= email (cdr (assoc "EMAIL" (elt contact 2))))
                           (throw 'contact contact))))))

    (unless contact
                (set-buffer (find-file-noselect (ido-completing-read
                                                 "Select org-contact file: "
                                                 org-contacts-files)))
                (goto-char (point-max))
                (insert (format  "\n* %s %s\n"
                                 (read-input "First name: ")
                                 (read-input "Last name: ")))
                (org-entry-put (point) "EMAIL" email)
                (save-buffer))

    (when contact
      (find-file  (cdr (assoc "FILE" (elt contact 2))))
      (goto-char (elt contact 1))
      (show-subtree))))


(defun org-contacts-tag-selection (selection)
  "Prompts you for a tag, and tags each entry in org-contacts
that has a matching email in `helm-marked-candidates'. Ignore
emails that are not in an org-contact file. I am not sure what
the best thing to do there is. Probably prompt for a file, and
add an entry to the end of it."
  (save-excursion
    (let ((tag (read-input "Tag: ")))
      (loop for email in (helm-marked-candidates)
            do
            (let ((contact (catch 'contact
                             (loop for contact in  (org-contacts-db)
                                   do
                                   (when (string=
                                          email
                                          (cdr (assoc
                                                "EMAIL"
                                                (elt contact 2))))
                                     (throw 'contact contact))))))
              ;; add new contact and tag it
              (unless contact
                (set-buffer (find-file-noselect (ido-completing-read
                                                 "Select org-contact file: "
                                                 org-contacts-files)))
                (goto-char (point-max))
                (insert (format  "\n* %s %s\n"
                                 (read-input "First name: ")
                                 (read-input "Last name: ")))
                (org-entry-put (point) "EMAIL" email)
                (org-set-tags-to (list tag))
                (save-buffer))
              ;; update tags on existing entry
              (when contact
                (find-file-noselect  (cdr (assoc "FILE" (elt contact 2))))
                (set-buffer (marker-buffer (elt contact 1)))
                (goto-char (elt contact 1))
                (org-set-tags-to (append (org-get-tags) (list tag)))))))))


(defun j-insert-emails ()
  "Helm interface to email addresses"
  (interactive)

  (helm :sources `(((name . "Email address candidates")
                    (candidates . ,(append
                                    ;; my aliases
                                    email-groups
                                    ;; org-contacts
                                    (loop for contact in (org-contacts-db)
                                          collect
                                          (cons (format
                                                 "%s %s %s <%s> org-contact"
                                                 (cdr (assoc "FIRSTNAME" (elt contact 2)))
                                                 (cdr (assoc "LASTNAME" (elt contact 2)))
                                                 (cdr (assoc "TAGS" (elt contact 2)))
                                                 (cdr (assoc "EMAIL" (elt contact 2))))
                                                (cdr (assoc "EMAIL" (elt contact 2)))))
                                    ;; mu contacts
                                    (loop for contact in mu4e~contacts-for-completion
                                          collect (cons contact contact))))
                    ;; only action is to insert string at point.
                    (action . (("insert" . (lambda (x)
                                             (insert
                                              (mapconcat
                                               'identity
                                               (helm-marked-candidates)
                                               ","))))
                               ("open" . org-contacts-open-from-email)
                               ("tag"  . org-contacts-tag-selection)))))))

;; Finally, let us bind this to something probably convenient. I use c-c ] for
;; citations. Lets try that in compose mode.
(define-key mu4e-compose-mode-map "\C-c]" 'j-insert-emails)
