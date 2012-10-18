(require 'psvn)

(defun steph:svn-log-ediff-with-current ()
  (interactive)
  (let* ((cur-buf (current-buffer))
         (diff-rev (svn-log-revision-for-diff))
         (upper-rev (if diff-rev
                        diff-rev
                      (svn-log-revision-at-point)))
         (lower-rev (if diff-rev
                        (svn-log-revision-at-point)
                      (number-to-string (- (string-to-number upper-rev) 1))))
         (file-name (svn-log-file-name-at-point t))
         (default-directory (svn-status-base-dir))
	 (svn-status-get-line-information-for-file 'relative)
	 (lower-rev-file-name (cdar (svn-status-get-specific-revision-internal
				     (list (svn-status-make-line-info file-name)) lower-rev nil)))
	 (ediff-after-quit-destination-buffer (current-buffer))
         (my-buffer (find-file-noselect file-name))
         (base-buff (find-file-noselect lower-rev-file-name))
         (svn-transient-buffers (list my-buffer base-buff))
	 (startup-hook '(svn-ediff-startup-hook)))
    (ediff-buffers base-buff my-buffer startup-hook)))

(define-key svn-log-view-mode-map (kbd "A") 'steph:svn-log-ediff-with-current)
