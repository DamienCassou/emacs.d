(add-hook 'ido-setup-hook
          (lambda ()
            ;; Go straight home
            (define-key ido-file-completion-map
              (kbd "`")
              (lambda ()
                (interactive)
                (if (looking-back "~/")
                    (insert ".emacs.d/")
                  (if (looking-back "/")
                      (insert "~/")
                    (call-interactively 'self-insert-command)))))))
