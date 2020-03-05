;;; Code


(eval-when-compile
  (require 'init-custom))


;; Emacs client for the Language Server Protocol
(use-package lsp-mode
  :ensure t
  :commands
  (lsp lsp-deferred)
  :hook
  ((go-mode ng2-html-mode) . lsp-deferred)
  :custom
  (lsp-clients-angular-language-server-command
   `("node"
     ,(kumo-home-path-resolve "/.config/yarn/global/node_modules/@angular/language-server")
     "--ngProbeLocations"
     ,(kumo-home-path-resolve "/.config/yarn/global/node_modules")
     "--tsProbeLocations"
     ,(kumo-home-path-resolve "/.config/yarn/global/node_modules")
     "--stdio"))
  (lsp-auto-guess-root nil)      ; not Detect project root
  (lsp-prefer-flymake nil)       ; Use lsp-ui and flycheck
  (lsp-enable-snippet t)
  :config
  (use-package lsp-ui
    :commands lsp-ui
    :after lsp-mode
    :custom
    ;; lsp-ui-doc
    (lsp-ui-doc-enable t)
    (lsp-ui-doc-header nil)
    (lsp-ui-doc-position 'bottom) ;; top, bottom, or at-point
    (lsp-ui-doc-max-width 120)
    (lsp-ui-doc-max-height 30)
    (lsp-ui-doc-use-childframe t)
    (lsp-ui-doc-use-webkit nil)
    (lsp-ui-doc-delay 0.2)
    ;; lsp-ui-flycheck

    (lsp-ui-flycheck-enable t)
    ;; lsp-ui-sideline
    (lsp-ui-sideline-enable t)
    (lsp-ui-sideline-ignore-duplicate t)
    (lsp-ui-sideline-show-symbol t)
    (lsp-ui-sideline-show-hover nil)
    (lsp-ui-sideline-show-diagnostics nil)
    (lsp-ui-sideline-show-code-actions t)
    (lsp-ui-sideline-code-actions-prefix " ")
    ;; lsp-ui-imenu
    (lsp-ui-imenu-enable nil)
    (lsp-ui-imenu-kind-position 'top)
    ;; lsp-ui-peek
    (lsp-ui-peek-enable nil)
    (lsp-ui-peek-peek-height 20)
    (lsp-ui-peek-list-width 50)
    (lsp-ui-peek-fontify 'on-demand)
    (lsp-use-native-json nil)
    )

  (use-package lsp-ivy
    :after lsp-mode
    :bind
    (:map lsp-mode-map
          ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)
          ("C-s-." . lsp-ivy-global-workspace-symbol)))

  (use-package company-lsp
    :custom
    (company-lsp-cache-candidates 'auto))

  ;; dap
  (use-package dap-mode
    :hook
    (lsp-mode . dap-mode)
    :bind
    (:map lsp-mode-map
          ("<f5>" . dap-debug)
          ("M-<f5>" . dap-hydra))
    :init
    (require 'dap-hydra)
    (require 'dap-go)
    :config
    (use-package dap-ui
      :ensure nil
      :hook
      (dap-mode . dap-ui-mode)))

  (use-package lsp-treemacs
    :bind
    (:map lsp-mode-map
          ("C-<f8>" . lsp-treemacs-errors-list)
          ("M-<f8>" . lsp-treemacs-symbols))
    :config
    (with-eval-after-load 'ace-window
      (when (boundp 'aw-ignored-buffers)
        (push 'lsp-treemacs-symbols-mode aw-ignored-buffers))))
  )


;; C/C++/Objective-C support
(use-package ccls
  :defines projectile-project-root-files-top-down-recurring
  :hook
  ((c-mode c++-mode objc-mode cuda-mode) . (lambda ()
                                             (require 'ccls)
                                             (lsp)))
  :custom
  (lsp-prefer-flymake nil)
  (flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  (ccls-initialization-options kumo/ccls-initialization-options)
  :config
  (with-eval-after-load 'projectile
    (setq projectile-project-root-files-top-down-recurring
          (append '("compile_commands.json"
                    ".ccls")
                  projectile-project-root-files-top-down-recurring))))


(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
