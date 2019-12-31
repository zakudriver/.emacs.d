;;; Code:

;; Emacs client for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode
;;(use-package lsp-mode
;;  :diminish lsp-mode
;;  :init
;;  (add-hook 'prog-major-mode #'lsp-prog-major-mode-enable)
;;  :config
;;  (use-package lsp-ui
;;    :defines lsp-ui-mode-map
;;    :commands (lsp-ui-mode lsp-ui-peek-find-definistions lsp-ui-peek-find-references)
;;    :bind (:map lsp-ui-mode-map
;;                ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
;;                ([remap xref-find-references] . lsp-ui-peek-find-references))
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :config
;;   (setq lsp-ui-sideline-enable nil))
;;    (use-package company-lsp
;;      :defines company-backends
;;      :functions company-backend-with-yas
;;      :init (add-to-list 'company-backends (company-backend-with-yas 'company-lsp))
;;      :after company
;;      :config
;;      (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)))


;; Emacs client for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :ensure t
  :hook ((typescript-mode web-mode) . lsp)
  :commands lsp

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
    ;; (lsp-ui-doc-enable nil)
    (lsp-ui-doc-header t)
    ;; (lsp-ui-doc-include-signature nil)
    (lsp-ui-doc-position 'bottom) ;; top, bottom, or at-point
    (lsp-ui-doc-max-width 120)
    (lsp-ui-doc-max-height 30)
    (lsp-ui-doc-use-childframe t)
    (lsp-ui-doc-use-webkit t)
    ;; lsp-ui-flycheck
    ;; (lsp-ui-flycheck-enable nil)
    ;; lsp-ui-sideline
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
   )

  (use-package company-lsp 
    :commands company-lsp
    :after company
  )

  ;; dap
  (use-package dap-mode
    :hook (lsp-mode . dap-mode)
    :config
    (dap-mode 1)
    (require 'dap-hydra)
    (require 'dap-go)
    (use-package dap-ui
      :ensure nil
      :config
      (dap-ui-mode t)))

  (use-package lsp-treemacs :commands lsp-treemacs-errors-list))



(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
