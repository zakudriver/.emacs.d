;;; Code:

;; Emacs client for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :diminish lsp-mode
  :init
  (add-hook 'prog-major-mode #'lsp-prog-major-mode-enable)
  :config
  (use-package lsp-ui
    :defines lsp-ui-mode-map
    :commands (lsp-ui-mode lsp-ui-peek-find-definistions lsp-ui-peek-find-references)
    :bind (:map lsp-ui-mode-map
                ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
                ([remap xref-find-references] . lsp-ui-peek-find-references))
   :hook (lsp-mode . lsp-ui-mode)
   :config
   (setq lsp-ui-sideline-enable nil))
    (use-package company-lsp
      :defines company-backends
      :functions company-backend-with-yas
      :init (add-to-list 'company-backends (company-backend-with-yas 'company-lsp))
      :after company
      :config
      (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)))

;; Go support for lsp-mode using Sourcegraph's Go Language Server
;; Install: go get github.com/sourcegraph/go-langserver
(use-package lsp-go
  :commands lsp-go-enable
  :hook (go-mode . lsp-go-enable))

;; Python support for lsp-mode using pyls.
;; Install: pip install python-language-server
(use-package lsp-python
  :commands lsp-python-enable
  :hook (python-mode . lsp-python-enable))

;; Javascript, Typescript and Flow support for lsp-mode
;; Install: npm i -g javascript-typescript-langserver
(use-package lsp-javascript-typescript
  :commands lsp-javascript-typescript-enable
  :hook ((typescript-mode js2-mode) . lsp-javascript-typescript-enable))

;; Java support for lsp-mode using the Eclipse JDT Language Server.
;; Install:
;; wget http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz
;; tar jdt-language-server-latest.tar.gz -C ~/.emacs.d/eclipse.jdt.ls/server/
(use-package lsp-java
  :commands lsp-java-enable
  :hook (java-mode . lsp-java-enable))

(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
