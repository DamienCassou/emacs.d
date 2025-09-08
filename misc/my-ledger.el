;;; my-ledger.el --- Helpful features for ledger-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>

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

;; Provide helpful features for ledger-mode including:
;; - creating transactions from mortgage transaction descriptions from the bank
;; - creating budget transactions to fill a wrongly-impacted budget

;;; Code:

;;; Move point in current buffer to insert new transaction at MOMENT

(let ((date-format "%A, %B %-e"))
  (defun my-ledger-position-at-date (moment)
    "Move point in current buffer to insert new transaction at MOMENT.
MOMENT is an encoded date."
    (let ((heading (format "*** %s" (format-time-string date-format moment))))
      (goto-char (point-min))
      (search-forward heading)
      (forward-line)
      (re-search-forward "; \\*\\*" nil t)
      (goto-char (line-beginning-position)))))

;;; Insert a mortgage transaction based on a given description

(defun my-ledger-insert-mortgage-transaction ()
  "Insert a transaction based on a given description."
  (interactive)
  (let* ((description (read-string "Description: "))
         (date (ledger-read-date "Date: ")))
    (my-ledger-position-at-date (ledger-parse-iso-date date))
    (insert (format "%s %s\n" date description))
    (insert (format "  asset:current:couple  0\n"))
    (insert (format "  expense:misc  0\n"))
    (save-excursion
      (my-ledger--mortgage-rewrite))))

(defun my-ledger--mortgage-rewrite ()
  "Rewrite the mortgage transaction at point."
  (when-let* ((numbers (my-ledger--mortgage-read-numbers))
              (total (seq-reduce
                      #'+
                      (map-values (my-ledger--mortgage-read-numbers))
                      0))
              (mortgage-type (my-ledger--mortgage-guess-type numbers)))
    (save-match-data
      (save-excursion
        (ledger-navigate-beginning-of-xact)
        (when (re-search-forward " .*$" (line-end-position)) ; skip date
          (replace-match " banque populaire prêt" t)
          (next-line)
          (delete-region (line-beginning-position) (line-end-position))
          (insert (format " asset:current:couple  %.2f" (- total)))
          (ledger-navigate-end-of-xact)
          (delete-region (line-beginning-position) (line-end-position))
          (map-do
           (lambda (number-type number)
             (when (> number 0)
               (insert
                (format
                 " expense:mortgage:%s%s  %s\n"
                 mortgage-type number-type number))))
           numbers)
          (delete-backward-char 1) ; remove additional newline
          (ledger-post-align-dwim))))))

(defun my-ledger--mortgage-guess-type (numbers)
  "Return the type of the transaction with NUMBERS.
The type is either 'ecoptz, 'immo1 or 'immo2.

NUMBERS is of the form (:capital CAPITAL :insurance INSURANCE :interest INTEREST)."
  (cond
   ((= (map-elt numbers :interest) 0) 'ecoptz)
   ((>= (map-elt numbers :capital) 410) 'immo1)
   (t 'immo2)))

(defun my-ledger--mortgage-read-numbers ()
  "Returns the numbers of the current mortgage reimbursement transaction.

The returned value is of the form (:capital CAPITAL :insurance INSURANCE :interest INTEREST)."
  (cl-labels ((parse-number (string) (string-to-number (string-replace "," "." string))))
    (let* ((number-regexp (rx (1+ (any digit)) ?, (1+ (any digit))))
           (regexp (rx "ECHEANCE PRET"
                       (? " -")
                       " DONT CAP "
                       (group-n 1 (regexp number-regexp))
                       " ASS. "
                       (group-n 2 (regexp number-regexp))
                       "E"
                       (? " -")
                       " INT. "
                       (group-n 3 (regexp number-regexp))
                       (? " COM. 0,00E"))))
      (save-match-data
        (save-excursion
          (ledger-navigate-beginning-of-xact)
          (when-let* (((re-search-forward regexp (line-end-position)))
                      (capital (parse-number (match-string 1)))
                      (insurance (parse-number (match-string 2)))
                      (interest (parse-number (match-string 3))))
            (list :capital capital :insurance insurance :interest interest)))))))

;;; Add a new budget transaction used to fill the wrongly-impacted budget of the current transaction

(defun my-ledger-fill-with-budget ()
  "Add a new budget transaction used to fill the wrongly-impacted budget of the current transaction."
  (interactive)
  (save-match-data
    (pcase-let* ((`(,begin ,end) (ledger-navigate-find-xact-extents (point))))
      (goto-char begin)
      (when-let* ((date
                   (progn
                     (looking-at ledger-full-date-regexp)
                     (match-string ledger-regex-full-date-group-actual)))
                  (expense-accounts-and-amounts
                   (cl-loop
                    while (re-search-forward ledger-post-line-regexp end t)
                    for account = (match-string ledger-regex-post-line-group-account)
                    for amount-string = (match-string ledger-regex-post-line-group-amount)
                    when (string-prefix-p "expense" account) collect (cons account
                                                                           (car (ledger-split-commodity-string  amount-string)))))
                  (budget-accounts
                   (seq-filter (lambda (account) (string-prefix-p "budget:" account)) (ledger-accounts-list)))
                  (source-budget
                   (ledger-completing-read-with-default "Choose a source budget"
                                                        "budget:vacation"
                                                        budget-accounts)))
        (goto-char end)
        (insert "\n\n")
        (insert date " budget\n")
        (insert "    " source-budget "\n")
        (pcase-dolist (`(,expense-account . ,expense-amount) expense-accounts-and-amounts)
          (insert "    " (my-ledger--find-corresponding-budget expense-account budget-accounts) "  " (number-to-string expense-amount) "\n"))))))

(defun my-ledger--find-corresponding-budget (expense-account budget-accounts)
  "Return the one of BUDGET-ACCOUNTS which closest ressemble EXPENSE-ACCOUNT."
  (when-let* ((expense-account-list (cdr (split-string expense-account ":")))
              (budget-accounts-list (mapcar (lambda (budget-account) (cdr (split-string budget-account ":"))) budget-accounts))
              (closest-budget-account-list (my-ledger--seq-min budget-accounts-list
                                                      (lambda (budget-account-list)
                                                        (my-ledger--levenstein budget-account-list expense-account-list)))))
    (string-join (cons "budget" closest-budget-account-list) ":")))

(defun my-ledger--seq-min (list fn)
  "Return the smallest in LIST according to FN.
FN should accept an element of LIST as argument and return a number."
  (let (result (result-value most-positive-fixnum))
    (dolist (elem list result)
      (let ((value (funcall fn elem)))
        (when (< value result-value)
          (setq result-value value)
          (setq result elem))))))

(defun my-ledger--levenstein (list1 list2)
  "Return the levenstein distance between list1 and list2, both list of strings."
  (cond
   ((seq-empty-p list2) (length list1))
   ((seq-empty-p list1) (length list2))
   ((string= (car list1) (car list2)) (my-ledger--levenstein (cdr list1) (cdr list2)))
   (t (1+ (min
           (my-ledger--levenstein (cdr list1) list2)
           (my-ledger--levenstein list1 (cdr list2))
           (my-ledger--levenstein (cdr list1) (cdr list2)))))))

(provide 'my-ledger)
;;; my-ledger.el ends here
