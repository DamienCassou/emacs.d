;;; 2-way synchronisation between AUCTeX from Emacs and Skim MacOSX PDF viewer
;;;
;;; - Install Skim.app first (PDF viewer)
;;; - In Skim preferences, 'Sync' tab, set:
;;;     - Preset: Custom
;;;     - Command: /usr/local/Cellar/emacs/24.2/bin/emacsclient (or similar)
;;;     - Arguments: --no-wait +%line "%file"
;;;
;;; - Now, you can use C-c C-c from Emacs to open Skim
;;;   and Shift+Command+Click in Skim to open Emacs

(server-start)

(eval-after-load "tex"
  '(progn
     (add-to-list 'TeX-expand-list '("%u" skim-make-url))
     (add-to-list 'TeX-view-program-list '("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %u"))
     (add-to-list 'TeX-view-program-selection '(output-pdf "Skim"))))


(defun skim-make-url ()
  (concat
   (TeX-current-line)
   " "
   (expand-file-name (funcall file (TeX-output-extension) t)
		     (file-name-directory (TeX-master-file)))
   " "
   (buffer-file-name)))


(provide 'auctex-skim-sync)
