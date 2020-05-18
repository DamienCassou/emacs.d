;; This function is in a dedicated file so lint-system can find it.

(defun my/ledger-clean-buffer ()
  "Same as `ledger-mode-clean-buffer' but limited to transactions.

Only transactions are ordered and formatted.

This is useful because the beginning of my buffer is formatted by
hand."
  (interactive)
  (let ((transactions-start (save-excursion
                              (goto-char (point-min))
                              (re-search-forward "^; \\* Transactions$")
                              (line-beginning-position)))
        (transactions-end (point-max)))
    (save-restriction
      (narrow-to-region transactions-start transactions-end)
      (ledger-mode-clean-buffer)
      (whitespace-cleanup))))
