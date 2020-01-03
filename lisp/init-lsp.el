;;; Code:


;; Emacs client for the Language Server Protocol
(use-package lsp-mode
  :ensure t
  :diminish lsp-mode
  :commands
  (lsp lsp-deferred)
  ;; :hook
  ;; ((typescript-mode web-mode go-mode) . lsp)
  :hook
  ((typescript-mode web-mode go-mode) . lsp)
  ((typescript-mode web-mode go-mode) . lsp-deferred)
  :custom
  (lsp-auto-guess-root t)
  (lsp-enable-snippet nil)
  (lsp-prefer-flymake nil)
  :custom
  ;; debug
  (lsp-print-io nil)
  (lsp-trace nil)
  (lsp-print-performance nil)
  :config
  (use-package lsp-ui 
    :commands lsp-ui-mode
    :hook (lsp-mode . lsp-ui-mode)
    :custom
    ;; lsp-ui-doc
    (lsp-ui-doc-enable t)
    (lsp-ui-doc-header t)
    ;; (lsp-ui-doc-include-signature nil)
    (lsp-ui-doc-position 'bottom) ;; top, bottom, or at-point
    (lsp-ui-doc-max-width 120)
    (lsp-ui-doc-max-height 30)
    (lsp-ui-doc-use-childframe t)
    (lsp-ui-doc-use-webkit t)
    ;; lsp-ui-flycheck
    ;; (lsp-ui-flycheck-enable nil)
    (lsp-ui-sideline-enable nil)
    (lsp-ui-sideline-ignore-duplicate t)
    (lsp-ui-sideline-show-symbol t)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-sideline-show-diagnostics nil)
    (lsp-ui-sideline-show-code-actions t)
    (lsp-ui-sideline-code-actions-prefix "")
    ;; lsp-ui-imenu
    (lsp-ui-imenu-enable t)
    (lsp-ui-imenu-kind-position 'top)
    ;; lsp-ui-peek
    (lsp-ui-peek-enable t)
    (lsp-ui-peek-peek-height 20)
    (lsp-ui-peek-list-width 50)
    (lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
    (lsp-use-native-json nil)
   )

  (use-package company-lsp 
    :commands company-lsp
    :after company
  )

  ;; dap
  (use-package dap-mode
    :hook (lsp-mode . dap-mode)
    :config
    (dap-mode t)
    (require 'dap-hydra)
    (require 'dap-go)
    (use-package dap-ui
      :ensure nil
      :config
      (dap-ui-mode t)))


  (use-package lsp-treemacs
    :bind (:map lsp-mode-map
                ("M-`" . lsp-treemacs-errors-list)))
)



(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
