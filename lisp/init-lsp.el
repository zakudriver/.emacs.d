;;; Code:


(eval-when-compile
  (require 'init-custom))


;; Emacs client for the Language Server Protocol
(use-package lsp-mode
  :commands
  (lsp-deferred)
  :hook
  (prog-mode . (lambda ()
                 (if (apply 'derived-mode-p kumo/lsp-major-mode)
                     (lsp-deferred))))
  (lsp-mode . (lambda ()
                (if (apply 'derived-mode-p kumo/lsp-on-save-major-mode)
                    (add-hook 'before-save-hook #'lsp-format-buffer t t))))
  :custom
  (lsp-clients-angular-language-server-command
   `("node"
     ,(kumo-home-path-resolve kumo/global-nodemodules-path "/@angular/language-server")
     "--ngProbeLocations"
     ,(kumo-home-path-resolve kumo/global-nodemodules-path)
     "--tsProbeLocations"
     ,(kumo-home-path-resolve kumo/global-nodemodules-path)
     "--stdio"))
  (lsp-auto-guess-root nil)      ; not Detect project root
  (lsp-log-io nil)
  (lsp-print-performance nil)
  (lsp-response-timeout 10)
  (lsp-idle-delay 0.500)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-folding nil)
  (lsp-enable-snippet nil)
  (lsp-enable-links nil)
  (lsp-enable-symbol-highlighting nil)
  ;; (lsp-restart 'auto-restart)
  (lsp-completion-show-detail nil)
  (lsp-completion-sort-initial-results t)
  (lsp-completion-use-last-result t)
  (lsp-signature-auto-activate nil)
  (lsp-enable-indentation nil)
  ;; deno
  (lsp-clients-deno-config "./tsconfig.json")
  (lsp-clients-deno-import-map "./import_map.json")
  (lsp-clients-deno-enable-lint nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  (lsp-lens-enable nil)
  (lsp-disabled-clients '((web-mode . (deno-ls)) (typescript-mode . (deno-ls))))
  ;; eslint
  (lsp-eslint-run "onSave")
  (lsp-eslint-format nil)
  ;; typescript/javascript
  (lsp-typescript-format-enable nil)
  (lsp-javascript-format-enable nil))

;; (use-package company-lsp
;;   :after (lsp-mode company)
;;   :custom
;;   (company-lsp-cache-candidates t)
;;   (company-lsp-filter-candidates t)
;;   (company-lsp-async t)
;;   :config
;;   (push 'company-lsp company-backends))

(use-package lsp-ivy
  :after lsp-mode
  :bind
  (:map lsp-mode-map
        ("s-l w w" . lsp-ivy-workspace-symbol)
        ("s-l w g" . lsp-ivy-global-workspace-symbol)))


(use-package lsp-ui
  :commands lsp-ui
  :after lsp-mode
  :bind
  (:map lsp-ui-mode-map
        ("s-l c a" . lsp-ui-sideline-apply-code-actions))
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
  (lsp-ui-doc-show-with-mouse nil)
  ;; lsp-ui-sideline
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-sideline-code-actions-prefix "💡 ")
  ;; lsp-ui-imenu
  (lsp-ui-imenu-enable t)
  (lsp-ui-imenu-kind-position 'top)
  (lsp-ui-imenu-auto-refresh 'after-save)
  ;; lsp-ui-peek
  (lsp-ui-peek-enable nil)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width 50)
  (lsp-ui-peek-fontify 'on-demand))


(use-package dap-mode
  :hook
  ((lsp-mode . dap-auto-configure-mode)
   (dap-stopped . (lambda (_args) (dap-hydra)))
   (dap-terminated . (lambda (_args) (dap-hydra/nil)))
   (go-mode . (lambda () (require 'dap-go)))
   ((c-mode c++-mode objc-mode swift-mode) . (lambda () (require 'dap-lldb)))
   (elixir-mode . (lambda () (require 'dap-elixir)))
   ((js-mode js2-mode) . (lambda () (require 'dap-chrome)))
   (ruby-mode . (lambda () (require 'dap-ruby))))
  :bind
  (:map lsp-mode-map
        ("C-. d d" . dap-debug)
        ("C-. d h" . dap-hydra))
  :init
  (require 'dap-hydra))


;; C/C++/Objective-C support
(use-package ccls
  :defines projectile-project-root-files-top-down-recurring
  :hook
  ((c-mode c++-mode objc-mode cuda-mode) . (lambda ()
                                             (require 'ccls)
                                             (lsp)))
  :custom
  (flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  (ccls-initialization-options kumo/ccls-initialization-options)
  :config
  (with-eval-after-load 'projectile
    (setq projectile-project-root-files-top-down-recurring
          (append '("compile_commands.json"
                    ".ccls")
                  projectile-project-root-files-top-down-recurring))))


;; dart
(use-package lsp-dart
  :custom
  (lsp-dart-outline nil)
  (lsp-dart-sdk-dir (kumo-home-path-resolve "/opt/flutter/bin/cache/dart-sdk"))
  (lsp-dart-flutter-sdk-dir (kumo-home-path-resolve "/opt/flutter")))


(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
