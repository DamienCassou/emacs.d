(require 'which-func)
(add-to-list 'which-func-modes 'org-mode)
(add-to-list 'which-func-modes 'latex-mode)
(add-to-list 'which-func-modes 'php-mode)

;; Fixes which-function
(defun which-func--trim (str)
  "Trim leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(defadvice which-function (around trim-which-function)
  ad-do-it
  (setf ad-return-value (which-func--trim ad-return-value)))
(ad-activate 'which-function)
