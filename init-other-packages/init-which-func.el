(require 'which-func)
(add-to-list 'which-func-modes 'org-mode)
(add-to-list 'which-func-modes 'latex-mode)
(add-to-list 'which-func-modes 'php-mode)

(defun which-func--limit (str)
  "Returns a string that is equal to STR if STR's length is lower
  than 20. Otherwise, returns the first 17 characters and append '...'."
  (let ((limit 20))
    (if (> (length str) limit)
	(concat (substring str 0 17) "...")
      str)))

(defun which-func--trim (str)
  "Trim leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(defun which-func--prepare (str)
  "Cleans STR so that is can be displayed nicely within the mode line."
  (which-func--limit (which-func--trim str)))

(defadvice which-function (around prepare-which-function)
  ad-do-it
  (setf ad-return-value (which-func--prepare ad-return-value)))
(ad-activate 'which-function)
