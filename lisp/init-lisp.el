;;; Code:


;; Show function arglist or variable docstring
(use-package eldoc
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
  :hook
  (ielm-mode . eldoc-mode))


(use-package highlight-quoted
  :hook
  (emacs-lisp-mode . highlight-quoted-mode))


(provide 'init-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lisp.el ends here
