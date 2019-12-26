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
  :ensure nil
  :hook ((typescript-mode web-mode) . lsp)
  :commands lsp
  :config
  (use-package lsp-ui 
    :commands lsp-ui-mode
    :hook (lsp-mode . lsp-ui-mode)
   )
  (use-package company-lsp 
    :commands company-lsp
    :after company
  )
  (use-package lsp-treemacs :commands lsp-treemacs-errors-list))



(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
