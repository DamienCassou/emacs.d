;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("notes" "|---+------------------+--------+--------+-------+-------+-------+------|\n|   | Groupe           | $1     | $2     |       |       |       |      |\n| ! |                  | PreExo | DerExo | Somme | Total | Bonus | Note |\n|---+------------------+--------+--------+-------+-------+-------+------|\n|   | Coefficients     |        |        |       |       |       |      |\n| # | Meilleures notes |        |        |     0 |     0 |       |    0 |\n| ^ |                  |        |        |   Max |       |       |      |\n|---+------------------+--------+--------+-------+-------+-------+------|\n| # | $0          |        |        |     0 |     0 |       |    0 |\n|---+------------------+--------+--------+-------+-------+-------+------|\n#+TBLFM: \\$5=vwsum($PreExo..$DerExo,@4$PreExo..@4$DerExo):: \\$6=($Somme/$Max)*20;%.0f:: \\$8=$Total+$Bonus\n" "Tableau de notes" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Thu Jan 30 14:55:59 2014
