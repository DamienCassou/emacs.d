;;; autoclose-shell.el --- Start a shell command and automatically close the buffer if successful  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Keywords: processes, terminals, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defcustom autoclose-shell-delay 2
  "Number of seconds after which to close a command's buffer."
  :type 'int
  :group 'autoclose-shell)

(defun autoclose-shell-start (name command)
  "Execute COMMAND and display its execution.
If COMMAND ends successfully, close the buffer after a few
seconds.

Use NAME, a string, to name the process and buffer.

COMMAND is a list starting with the program file name, followed
by strings to give to the program as arguments."
  (let ((output-buffer (get-buffer-create name)))
    (display-buffer output-buffer)
    (make-process
     :name name
     :command command
     :buffer output-buffer
     :sentinel #'autoclose-shell--sentinel)))

(defun autoclose-shell--sentinel (proc event)
  "Close PROC's buffer after `autoclose-shell-delay' if EVENT indicates success."
  (when (string= event "finished\n")
    (let ((output-buffer (process-buffer proc)))
      (run-at-time
       autoclose-shell-delay
       nil ; don't repeat
       (lambda ()
         (when (buffer-live-p output-buffer)
           (kill-buffer output-buffer)))))))

(provide 'autoclose-shell)
;;; autoclose-shell.el ends here
