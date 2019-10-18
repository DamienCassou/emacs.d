;;; rewrite-bread.el --- Utility to rewrite a personal document  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Keywords:

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

(require 'rx)

(defun rewrite-bread-initial-cleanup ()
  "Prepare buffer for batch changes."
  (setf (point) (point-min))
  (delete-matching-lines (rx
                          (or
                           (and line-start line-end)
                           (and line-start "Livreur"))))
  ;; Add final newline if necessary:
  (setf (point) (point-max))
  (unless (eq (point) (line-beginning-position))
    (insert "\n")))

(defun rewrite-bread-copy-date-in-region (start end)
  (save-restriction
    (narrow-to-region start end)
    (setf (point) (point-min))
    (let ((date (buffer-substring-no-properties (point) (line-end-position))))
      (delete-region (point) (1+ (line-end-position)))
      (while (not (eobp))
        (insert "| " date " | ")
        (just-one-space)
        (next-logical-line)
        (setf (point) (line-beginning-position))))))

(defun rewrite-bread-copy-dates ()
  (setf (point) (point-min))
  (while (not (eobp))
    (let ((start (point))
          (end (save-excursion
                 (next-logical-line)
                 (re-search-forward (rx line-start
                                        (or
                                         (any "M" "V")
                                         line-end)))
                 (when (looking-back (rx (any "M" "V")))
                   (backward-char))
                 (point))))
      (rewrite-bread-copy-date-in-region start end))))

(defun rewrite-bread-name-to-column ()
  (setf (point) (point-min))
  (while (and (not (eobp))
              (re-search-forward "(" (line-end-position)))
    (replace-match " | ")
    (re-search-forward ")" (line-end-position))
    (replace-match " |")
    (next-logical-line)
    (setf (point) (line-beginning-position))))

(defun rewrite-bread-sort-table-by-name ()
  (setf (point) (point-min))
  (orgtbl-mode)
  (org-table-next-field)
  (org-table-next-field)
  (org-table-next-field)
  (org-table-sort-lines nil ?a))

(defun rewrite-bread-move-names-to-first-column ()
  (setf (point) (point-min))
  (org-table-next-field)
  (org-table-next-field)
  (org-table-next-field)
  (org-table-move-column-left)
  (org-table-move-column-left))

(defun rewrite-bread-export ()
  (org-table-export "/tmp/babel.csv" "orgtbl-to-csv")
  (message "/tmp/babel.csv generated"))

(defun rewrite-bread-convert-all ()
  (interactive)
  (rewrite-bread-initial-cleanup)
  (rewrite-bread-copy-dates)
  (rewrite-bread-name-to-column)
  (rewrite-bread-sort-table-by-name)
  (rewrite-bread-move-names-to-first-column)
  (rewrite-bread-export))

(provide 'rewrite-bread)
;;; rewrite-bread.el ends here
