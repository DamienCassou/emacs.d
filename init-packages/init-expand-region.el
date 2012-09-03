(require 'expand-region-custom)

(defcustom er/temporary-binding-expand "="
  "Key to use after an initial expand/contract to expand once more."
  :group 'expand-region)

(defcustom er/temporary-binding-contract "-"
  "Key to use after an initial expand/contract to contract once more."
  :group 'expand-region)

(defcustom er/temporary-binding-reset "0"
  "Key to use after an initial expand/contract to undo all."
  :group 'expand-region)

(defun er/prepare-for-more-expansions ()
  "Let one expand more by just pressing the last key.
- < C-x = = > will expand two times
- < C-x = = - > will expand two times and then undo one"
  (let* ((message
	  (format "Press %s to expand, %s to contract, and %s to undo"
		  er/temporary-binding-expand
		  er/temporary-binding-contract
		  er/temporary-binding-reset)))
    (set-temporary-overlay-map
     (let ((map (make-sparse-keymap))
	   (bindings (list (cons er/temporary-binding-expand
				  '(er/expand-region 1))
			   (cons er/temporary-binding-contract
				 '(er/contract-region 1))
			   (cons er/temporary-binding-reset
				 '(er/expand-region 0)))))
       (dolist (binding bindings map)
	 (define-key map (kbd (car binding))
	   `(lambda ()
	      (interactive)
	      (eval `,(cdr ',binding))
	      (message ,message)))))
     t)
    (message message)))

(global-unset-key (kbd "M-@"))
(global-set-key (kbd "C-x =")
		(lambda ()
		  (interactive)
		  (call-interactively 'er/expand-region)
		  (er/prepare-for-more-expansions)))
