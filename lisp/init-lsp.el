;;; Code:


;; Emacs client for the Language Server Protocol
(use-package lsp-mode
  :ensure t
  :diminish 
  :commands 
  (lsp lsp-deferred)
  :hook
  ((typescript-mode web-mode go-mode) . lsp)
  ((typescript-mode web-mode go-mode) . lsp-deferred)
  :custom
  (lsp-auto-guess-root t)
  (lsp-enable-snippet nil)
  (lsp-prefer-flymake nil)
  ;; debug
  (lsp-print-io nil)
  (lsp-trace nil)
  (lsp-print-performance nil)
  :config
  (use-package lsp-ui 
    :commands lsp-ui
    ;; :hook (lsp-mode . lsp-ui)
    :custom
    ;; lsp-ui-doc
    (lsp-ui-doc-enable t)
    (lsp-ui-doc-header t)
    ;; (lsp-ui-doc-include-signature nil)
    (lsp-ui-doc-position 'bottom) ;; top, bottom, or at-point
    (lsp-ui-doc-max-width 120)
    (lsp-ui-doc-max-height 30)
    (lsp-ui-doc-use-childframe t)
    (lsp-ui-doc-use-webkit nil)
    (lsp-ui-doc-delay 0.2)
    ;; lsp-ui-flycheck
    ;; (lsp-ui-flycheck-enable nil)
    ;; lsp-ui-sideline
    (lsp-ui-sideline-enable t)
    (lsp-ui-sideline-ignore-duplicate t)
    (lsp-ui-sideline-show-symbol t)
    (lsp-ui-sideline-show-hover nil)
    (lsp-ui-sideline-show-diagnostics nil)
    (lsp-ui-sideline-show-code-actions t)
    (lsp-ui-sideline-code-actions-prefix " ")
    ;; lsp-ui-imenu
    (lsp-ui-imenu-enable t)
    (lsp-ui-imenu-kind-position 'top)
    ;; lsp-ui-peek
    (lsp-ui-peek-enable t)
    (lsp-ui-peek-peek-height 20)
    (lsp-ui-peek-list-width 50)
    (lsp-ui-peek-fontify 'on-demand)
    (lsp-use-native-json nil)
   )

  (use-package lsp-ivy
    :after lsp-mode
    :bind (:map lsp-mode-map
                ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)
                ("C-s-." . lsp-ivy-global-workspace-symbol)))

  (use-package company-lsp 
    :init (setq company-lsp-cache-candidates 'auto)
    :config (push 'company-lsp company-backends))

  ;; dap
  (use-package dap-mode
    :bind (:map lsp-mode-map
                ("<f5>" . dap-debug)
                ("M-<f5>" . dap-hydra))
    :hook
    (go-mode . (lambda () (require 'dap-go)))
    ;; (lsp-mode . dap-mode)
    :init
    (require 'dap-hydra)
    :config
    (use-package dap-ui
      :ensure nil
      :config
      (dap-ui-mode t)))


  (use-package lsp-treemacs
    :bind (:map lsp-mode-map
                ("C-<f8>" . lsp-treemacs-errors-list)
                ("M-<f8>" . lsp-treemacs-symbols))
    :config
    (with-eval-after-load 'ace-window
      (when (boundp 'aw-ignored-buffers)
        (push 'lsp-treemacs-symbols-mode aw-ignored-buffers))))
)



(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
