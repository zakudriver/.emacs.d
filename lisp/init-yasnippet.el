(use-package yasnippet
  :diminish yas-minor-mode
  :init (add-hook 'prog-mode-hook #'yas-global-mode))

(use-package yasnippet-snippets)

(provide 'init-yasnippet)
