;;; helloasso.el --- Convert CSV from hello asso into a CSV we can use   -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Damien Cassou

;; Authors: Damien Cassou <damien@cassou.me>
;; Version: 0.0.1
;; URL: https://github.com/DamienCassou/beginend
;; Package-Requires: ((emacs "27.1"))
;; Created: 4 April 2022

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Take a CSV buffer from hello asso and rewrite it so we can use it.

;;; Code:


(require 'csv)

(defun helloasso-main (&optional buffer)
  "Rewrite CSV BUFFER to contain a CSV table in a different format."
  (interactive)
  (let* ((person-to-purchases (helloasso-read-buffer (or buffer (current-buffer))))
         (all-products (helloasso-get-products person-to-purchases)))
    (erase-buffer)
    (helloasso-write-line (cons "Personne" all-products))
    (helloasso-write-purchases person-to-purchases all-products)))

(defun helloasso-write-purchases (person-to-purchases all-products)
  "Insert CSV lines in current buffer for purchases in PERSON-TO-PURCHASES.
ALL-PRODUCTS is a list of strings for columns in the CSV."
  (dolist (person (map-keys person-to-purchases))
    (helloasso-write-person-purchase person (map-elt person-to-purchases person) all-products)))

(defun helloasso-write-person-purchase (person purchases all-products)
  "Insert CSV line in current buffer for PERSON's PURCHASES.
ALL-PRODUCTS is a list of strings for columns in the CSV."
  (let ((amounts (mapcar
                  (lambda (product) (map-elt purchases product ""))
                  all-products)))
    (helloasso-write-line (cons person amounts))))

(defun helloasso-write-line (line)
  "Insert LINE, a list of strings, into the current buffer."
  (dolist (string line)
    (insert (format "\"%s\";" string)))
  (insert "\n"))

(defun helloasso-get-products (person-to-purchases)
  "Return the list of all products in PERSON-TO-PURCHASES.
PERSON-TO-PURCHASES is a hash table mapping a product name to
the number a person bought."
  (let ((products-table (make-hash-table :test 'equal)))
    (dolist (person-purchases (map-values person-to-purchases) (map-keys products-table))
      (dolist (purchase (map-keys person-purchases))
        (map-put! products-table purchase t)))))

(defun helloasso-read-buffer (buffer)
  "Read BUFFER and return a hash table mapping person names to his/her purchases.
The purchases is another hash table mapping the product name to
the number the person bought."
  (let* ((lines (csv-parse-buffer t buffer))
         ;; Map a person's name to the stuff s·he bought
         (result (make-hash-table :test 'equal)))
    (dolist (line lines result)
      (let ((person-name (helloasso-read-person-name line)))
        (helloasso-add-purchase result person-name (helloasso-read-purchase line))))))

(defun helloasso-add-purchase (purchases person purchase)
  "Add PURCHASE to the person PERSON in PURCHASES."
  (let ((person-purchases (helloasso-get-person-purchases purchases person)))
    (map-put! person-purchases purchase (1+ (map-elt person-purchases purchase 0)))))

(defun helloasso-get-person-purchases (purchases person)
  "Return the purchases that PERSON bought in PURCHASES."
  (or (map-elt purchases person)
      (let ((new-person-purchases (make-hash-table :test 'equal)))
        (map-put! purchases person new-person-purchases)
        new-person-purchases)))

(defun helloasso-read-person-name (line)
  "Return a string representing the person in LINE."
  (format "%s %s"
          (substring-no-properties (map-elt line "Prénom"))
          (substring-no-properties (map-elt line "Nom"))))

(defun helloasso-read-purchase (line)
  "Return a string representing the product bought in LINE."
  (format "%s (%s€)"
          (substring-no-properties (map-elt line "Tarif")) ;; the product's name
          (substring-no-properties (map-elt line "Montant (€)")))) ;; the cost

(provide 'helloasso)
;;; helloasso.el ends here

;; Local Variables:
;; eval: (flycheck-mode)
;; End:
