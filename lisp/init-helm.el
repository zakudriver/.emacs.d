;;; Code:

(use-package helm
  :init
  (add-hook 'after-init-hook #'(lambda()
                                 (helm-mode 1)))
  :bind (
         :map helm-map
         ([escape] . helm-keyboard-quit)
         )
  :config
  (setq helm-recentf-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-locate-fuzzy-match t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-semantic-fuzzy-match t)
  (setq helm-imenu-fuzzy-match t)
  (setq helm-apropos-fuzzy-match t)
  (setq helm-lisp-fuzzy-completion t)
  (setq helm-session-fuzzy-match t)
  (setq helm-etags-fuzzy-match t)
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
  (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action) ; rebind tab to do persistent action
  (define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z

  (setq helm-split-window-in-side-p           t) ; open helm buffer inside current window, not occupy whole other window
  (setq helm-move-to-line-cycle-in-source     t) ; move to end or beginning of source when reaching top or bottom of source.
  (helm-autoresize-mode 1)

  (use-package helm-ag)
  (evil-define-key '(normal visual) helm-ag-mode-map (kbd "TAB") 'helm-ag-mode-jump-other-window)
  (evil-define-key '(normal visual) helm-ag-mode-map (kbd "<tab>") 'helm-ag-mode-jump-other-window)
  (evil-define-key '(normal visual) helm-ag-mode-map (kbd "<return>") 'helm-ag-mode-jump)
  (evil-define-key '(normal visual) helm-ag-mode-map (kbd "RET") 'helm-ag-mode-jump)
  
  (use-package helm-ls-git)
  (use-package helm-etags-plus)
  (use-package helm-swoop
    :config
    ;; Disable pre-input
    (setq helm-swoop-pre-input-function
          (lambda () ""))))

(provide 'init-helm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-helm.el ends here
