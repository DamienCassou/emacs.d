(defun reb-query-replace (to-string)
  "Replace current RE from point with `query-replace-regexp'."
  (interactive
   (progn (barf-if-buffer-read-only)
          (list (query-replace-read-to (Footarget-binding Fooregexp)
                                       "Query replace"  t))))
  (reb-quit)
  (query-replace-regexp (reb-target-binding reb-regexp) to-string))

(define-key reb-mode-map (kbd "C-c M-%") 'reb-query-replace)
