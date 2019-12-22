(add-hook 'prog-mode-hook #'(lambda() (modify-syntax-entry ?_ "w")))

(use-package vimrc-mode)

(provide 'init-prog)
