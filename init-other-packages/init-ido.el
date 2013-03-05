(add-hook 'ido-setup-hook
          (lambda ()
            "Press ` to easily go to ~ and ~/.emacs.d"
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
