;;; youtube.el --- Facilitate viewing youtube videos from Emacs

;;; Commentary:
;;
(require 'thingatpt)

(defun youtube-url-at-point ()
  "Return the URL at point, nil if none found."
  (thing-at-point-url-at-point))

(defun youtube-play-video (url)
  "Play youtube video at URL locally."
  (interactive
   (let ((url-at-point (youtube-url-at-point)))
     (list
      (read-string "URL of youtube video: " url-at-point nil url-at-point))))
  (shell-command
   (format "youtube-dl -o - %s | vlc -" url)))

;;; Code:

(provide 'youtube)

;;; youtube.el ends here
