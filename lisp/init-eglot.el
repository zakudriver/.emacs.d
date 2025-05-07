;; init-eglot.el --- Initialize eglot configurations.	-*- lexical-binding: t -*-


;;; Commentary:
;; some configuration of eglot.

;;; Code:


(eval-when-compile
  (require 'init-custom))


(use-package eglot
  :defer 3
  :hook
  (prog-mode . (lambda ()
                 (if (apply 'derived-mode-p my/eglot-major-mode)
                     (eglot-ensure))))
  :custom
  (eglot-events-buffer-size 0)
  (eglot-autoshutdown       t)
  (eglot-menu-string        "⌨")
  :config
  ;; (add-to-list 'eglot-server-programs '((tsx-ts-mode :language-id "typescriptreact") . ("tailwindcss-language-server")))
  ;; (add-to-list 'eglot-server-programs
  ;;              '((typescript-ts-mode  :language-id "html") . ("tailwindcss-language-server" "--stdio")))

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



(use-package add-node-modules-path
  :hook
  (prog-mode . (lambda ()
                 (when (cl-position major-mode my/eslint-enable-mode :test 'eq)
                   (add-node-modules-path))))
  :custom
  (add-node-modules-path-command '("pnpm bin" "pnpm bin -w")))

(provide 'init-eglot)

;;; init-eglot.el ends here
