;; init-eglot.el --- Initialize eglot configurations.	-*- lexical-binding: t -*-


;;; Commentary:
;; some configuration of eglot.

;;; Code:


(eval-when-compile
  (require 'init-custom))


(use-package eglot
  :ensure nil
  :defer 3
  ;; :hook
  ;; (prog-mode . (lambda ()
  ;;                (when (apply 'derived-mode-p my/eglot-major-mode)
  ;;                  (eglot-ensure))))
  :custom
  (eglot-events-buffer-size 0)
  (eglot-autoshutdown       t)
  (eglot-menu-string        "⌨")
  :init
  (dolist (mode my/eglot-major-mode)
    (let ((hook-name (intern (concat (symbol-name mode) "-hook"))))
      (add-hook hook-name #'eglot-ensure)))
  :config
  (setq-default eglot-workspace-configuration
                '((:rust-analyzer . (:cargo (:allFeatures t)))))

  ;; (add-to-list 'eglot-server-programs '((tsx-ts-mode :language-id "typescriptreact") . ("tailwindcss-language-server")))
  ;; (add-to-list 'eglot-server-programs
  ;;              '((typescript-ts-mode  :language-id "html") . ("tailwindcss-language-server" "--stdio")))
  (defun my/setup-eglot-server-programs ()
    "Dynamically add some lsp servers before `eglot-ensure`."
    (dolist (mode my/oxlint-enable-mode)
      (add-to-list 'eglot-server-programs
                   `(,mode . ("oxc_language_server")))))
  ;; (my/setup-eglot-server-programs)

  (use-package consult-eglot
    :bind
    (:map eglot-mode-map
          ("C-M-." . consult-eglot-symbols))))


(use-package eglot-booster
  :load-path "~/.emacs.d/site-lisp/eglot-booster"
	:after eglot
	:config	(eglot-booster-mode))


(use-package eldoc
  :ensure nil
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p   nil))

(setq max-mini-window-height 1)


(use-package eldoc-box
  :after (eglot eldoc)
  :bind
  (:map eglot-mode-map
        ("C-c C-d" . eldoc-box-help-at-point))
  :custom
  (eldoc-box-max-pixel-height 600)
  (eldoc-box-max-pixel-width  800)
  (eldoc-box-clear-with-C-g   t))


;; (defvar my/after-node-modules-path-hook nil
;;   "Hook to run after add-node-modules-path.")


;; (add-hook 'eglot-managed-mode-hook
;;           (lambda ()
;;             (when (executable-find "oxc_language_server")
;;               (dolist (mode my/oxlint-enable-mode)
;;                 (add-to-list 'eglot-server-programs
;;                              `(,mode . ("oxc_language_server")))))))


;; (add-hook 'my/after-node-modules-path-hook
;;           (lambda ()
;;             (when (executable-find "oxc_language_server")
;;               (dolist (mode my/oxlint-enable-mode)
;;                 (add-to-list 'eglot-server-programs
;;                              `(,mode . ("oxc_language_server")))))))


(provide 'init-eglot)

;;; init-eglot.el ends here
