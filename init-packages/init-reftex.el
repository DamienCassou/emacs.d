(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(eval-after-load "reftex-vars"
  '(progn
     (add-to-list 'reftex-bibliography-commands "bibliographyphd")
     (add-to-list 'reftex-bibliography-commands "bibliographysoft")))
