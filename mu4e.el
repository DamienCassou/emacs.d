(add-to-list 'load-path "~/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)

(setq mu4e-mu-binary "~/usr/bin/mu")
(setq mu4e-user-mail-address-regexp "^damien.test.cassou.*")
(setq mu4e-headers-fields
      '((:date . 25) (:flags . 6) (:from-or-to . 22) (:subject)))
(setq mu4e-headers-date-format "%x %H:%M")

(setq mu4e-maildir       "~/Mail/GmailTest")
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save messages to Sent Messages, Gmail/IMAP will take care of this
(setq mu4e-sent-messages-behavior 'delete)


;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)
	 ("/[Gmail].Sent Mail"   . ?s)
	 ("/[Gmail].Trash"       . ?t)
	 ("/[Gmail].All Mail"    . ?a)))

;; allow for updating mail using 'U' in the main view:
;; (setq mu4e-get-mail-command "offlineimap")

;; something about ourselves
(setq
 user-mail-address "damien.test.cassou@gmail.com"
 user-full-name  "Damien Cassou"
 message-signature
 (concat
  "Foo X. Bar\n"
  "http://www.example.com\n"))

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

(require 'smtpmail)
(setq
 message-send-mail-function 'smtpmail-send-it
 smtpmail-stream-type 'starttls
 smtpmail-default-smtp-server "smtp.gmail.com"
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
