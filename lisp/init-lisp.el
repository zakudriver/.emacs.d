;; Show function arglist or variable docstring
(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :init
  ;; Enable Eldoc in lisp modes in 24
  ;; `global-eldoc-mode' is enabled by default in 25.
  (unless (fboundp 'global-eldoc-mode)
    (dolist (hook '(emacs-lisp-mode-hook
                    lisp-interaction-mode-hook
                    ielm-mode-hook
                    eval-expression-minibuffer-setup-hook))
      (add-hook hook #'eldoc-mode))))

(use-package ielm
  :config
  (add-hook 'ielm-mode-hook #'eldoc-mode))

(provide 'init-lisp)
