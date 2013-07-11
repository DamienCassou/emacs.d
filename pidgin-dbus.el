(defcustom org-pomodoro-pidgin-busy-status
  "Do not distrub pomodoro (%s:%s remaining)"
  "Status message when a pomodoro is in progress")

(defcustom org-pomodoro-pidgin-break-status
  "Available -- pomodoro break (%s:%s remaining)"
  "Status message during a break between 2 pomodoros")

(defun org-pompid--call-method (method &rest args)
  (apply
   #'dbus-call-method
   :session
   "im.pidgin.purple.PurpleService"
   "/im/pidgin/purple/PurpleObject"
   "im.pidgin.purple.PurpleInterface"
   method
   args))

(defun org-pompid--all-statuses ()
  (org-pompid--call-method "PurpleSavedstatusesGetAll"))

(defun org-pompid--status-title (status)
  (org-pompid--call-method "PurpleSavedstatusGetTitle" :int32 status))

(defun org-pompid--status-message (status)
  (org-pompid--call-method "PurpleSavedstatusGetMessage" :int32 status))

(defun org-pompid--set-status-message (status message)
  (org-pompid--call-method
   "PurpleSavedstatusSetMessage"
   :int32 status
   message))

(defun org-pompid--status-type-to-id (type)
  (case type
    (offline 1)
    (available 2)
    (unavailable 3)
    (invisible 4)
    (away 5)
    (extended AY = 6)
    (mobile 7)
    (tune 8)))

(defun org-pompid-gboolean-to-boolean (gboolean)
  (= gboolean 1))

(defun org-pompid--transient-p (status)
  (org-pompid-gboolean-to-boolean
   (org-pompid--call-method
    "PurpleSavedstatusIsTransient"
    :int32 status)))

(defun org-pompid--new-transient-status (type)
  (org-pompid--call-method
   "PurpleSavedstatusNew"
   ""
   :int32 (org-pompid--status-type-to-id type)))

(defun org-pompid--activate (status)
  (org-pompid--call-method
   "PurpleSavedstatusActivate"
   :int32 status))

(defun org-pompid--create-transient-status (type message)
  (let ((status (org-pompid--new-transient-status type)))
    (org-pompid--set-status-message status message)
    status))

(defun org-pompid--find-transient-status (regexp)
  (car
   (member-if
    (lambda (status)
      (and
       (string-match-p regexp (org-pompid--status-message status))
       (org-pompid--transient-p status)))
    (org-pompid--all-statuses))))

(defun org-pompid--find-or-create-transient-status (message type)
  (or
   (org-pompid--find-transient-status (format message ".*" ".*"))
   (org-pompid--create-transient-status 'unavailable message)))

(defun org-pompid--get-busy-status ()
  (org-pompid--find-or-create-transient-status
   org-pomodoro-pidgin-busy-status
   'unavailable))

(defun org-pompid--get-break-status ()
  (org-pompid--find-or-create-transient-status
   org-pomodoro-pidgin-break-status
   'available))

(defun org-pompid--format-status-message (remaining-seconds message)
  (format
   message
   (format "%02d" (/ remaining-seconds 60))
   (format "%02d" (% remaining-seconds 60))))

(defun org-pompid--update-status (remaining-seconds status message)
  (org-pompid--set-status-message
   status
   (org-pompid--format-status-message
    remaining-seconds
    message)))

(defun org-pomodoro-pidgin-update-busy-status (remaining-seconds)
  (org-pompid--update-status
   remaining-seconds
   (org-pompid--get-busy-status)
   org-pomodoro-pidgin-busy-status))

(defun org-pomodoro-pidgin-update-break-status (remaining-seconds)
  (org-pompid--update-status
   remaining-seconds
   (org-pompid--get-break-status)
   org-pomodoro-pidgin-break-status))

(defun org-pomodoro-pidgin-start-pomodoro ()
  (org-pompid--activate (org-pompid--get-busy-status)))

(defun org-pomodoro-pidgin-start-break ()
  (org-pompid--activate (org-pompid--get-break-status)))
