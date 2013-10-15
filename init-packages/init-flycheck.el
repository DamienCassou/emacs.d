(autoload 'flycheck-mode-on-safe "flycheck")

(defvar-local dc:activate-flycheck t
  "Decide if a file should be checked by flycheck.")
(defun dc:activate-flycheck ()
  (when dc:activate-flycheck
    (flycheck-mode-on-safe)))

(add-hook 'find-file-hook #'dc:activate-flycheck)

;; Local Variables:
;; dc:activate-flycheck: nil
;; End:
