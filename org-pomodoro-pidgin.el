;; -*- lexical-binding: t; -*-
;;; org-pomodoro-pidgin.el --- Integrate org-pomodoro and Pidgin
;;
;; Copyright (C) 2013 Damien Cassou
;;
;; Author: Damien Cassou <damien.cassou@gmail.com>
;; Created: 2013-07-11
;; Keywords: emacs package elisp pidgin pomodoro org-mode
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; commentary:
;;
;; Update Pidgin status as the Pomodoro progresses.
;;
;; (require 'org-pomodoro-pidgin)
;;
;;; code:
;;
;; The following code uses the "org-pompid" prefix for all "private"
;; functions (and an addition dash "-") and the "org-pomodoro-pidgin"
;; prefix for all "public" functions.

(require 'org-pomodoro)

(defcustom org-pomodoro-pidgin-busy-status
  "Do not distrub pomodoro (%s minutes remaining)"
  "Status message when a pomodoro is in progress.
The string will be passed to `format' with a number of remaining
  minutes.")

(defcustom org-pomodoro-pidgin-break-status
  "Available -- pomodoro break (%s remaining)"
  "Status message when a pomodoro is in progress.
The string will be passed to `format' with a number of remaining
  minutes.")

(defun org-pompid--status-type-to-id (type)
  (case type
    (offline 1)
    (available 2)
    (unavailable 3)
    (invisible 4)
    (away 5)
    (mobile 7)
    (tune 8)))

(defun org-pompid--call-method (method handler &rest args)
  (apply
   #'dbus-call-method-asynchronously
   :session
   "im.pidgin.purple.PurpleService"
   "/im/pidgin/purple/PurpleObject"
   "im.pidgin.purple.PurpleInterface"
   method
   handler
   args))

(defun org-pompid--set-status-message (status message)
  (org-pompid--call-method
   "PurpleSavedstatusSetMessage"
   nil
   :int32 status
   message))

(defun org-pompid--new-transient-status (type handler)
  (org-pompid--call-method
   "PurpleSavedstatusNew"
   handler
   ""
   :int32 (org-pompid--status-type-to-id type)))

(defun org-pompid--activate (status)
  (org-pompid--call-method
   "PurpleSavedstatusActivate"
   nil
   :int32 status))

(defun org-pompid--change-status-message (type message)
  (org-pompid--new-transient-status
   type
   (lambda (status)
     (org-pompid--set-status-message status message)
     (org-pompid--activate status))))

(defvar org-pompid--timer nil)
(defvar org-pompid--timer-function nil)

(defun org-pompid--tick ()
  (funcall org-pompid--timer-function))

(defun org-pompid--stop-timer ()
  (when org-pompid--timer
    (cancel-timer org-pompid--timer)
    (setq org-pompid--timer nil)))

(defun org-pompid--start-timer (function)
  (org-pompid--stop-timer)
  (setq
   org-pompid--timer-function function
   org-pompid--timer (run-at-time nil 30 #'org-pompid--tick)))

(defun org-pompid--update-busy-status ()
  (org-pompid--change-status-message
   'available
   (format org-pomodoro-pidgin-busy-status org-pomodoro-countdown)))

(defun org-pompid--update-break-status ()
  (org-pompid--change-status-message
   'available
   (format org-pomodoro-pidgin-break-status org-pomodoro-countdown)))

(add-hook
 'org-pomodoro-started-hook
 (lambda ()
   (org-pompid--start-timer #'org-pompid--update-busy-status)))

(add-hook
 'org-pomodoro-finished-hook
 (lambda ()
   (org-pompid--start-timer #'org-pompid--update-break-status)))

(add-hook
 'org-pomodoro-killed-hook
 (lambda ()
   (org-pompid--start-timer #'org-pompid--update-break-status)))

(provide 'org-pomodoro-pidgin)
;;; org-pomodoro-pidgin.el ends here
