(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(require 'magit-svn)
(add-hook 'magit-mode-hook 'turn-on-magit-svn)
