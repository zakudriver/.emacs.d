;; init-flymake.el --- Initialize flymake configurations.	-*- lexical-binding: t -*-


;;; Commentary:
;; some configuration of flymake.

;;; Code:


(require 'seq)


(use-package flymake
  :diminish
  :bind
  ("C-c c c" . flymake-show-buffer-diagnostics)
  :custom
  (flymake-no-changes-timeout        nil)
  (flymake-fringe-indicator-position 'right-fringe)
  (flymake-suppress-zero-counters    :note)
  :config
  (setq elisp-flymake-byte-compile-load-path
        (append elisp-flymake-byte-compile-load-path load-path)))


(use-package sideline-flymake
  :diminish sideline-mode
  :hook
  (flymake-mode . sideline-mode)
  :custom
  (sideline-flymake-display-mode 'point)
  (sideline-backends-right '(sideline-flymake)))


(use-package flymake-eslint
  :after eglot
  :hook
  (eglot-managed-mode . flymake-eslint-enable)
  :custom
  (flymake-eslint-prefer-json-diagnostics t))


(use-package flymake-oxlint
  :load-path "~/.emacs.d/site-lisp/flymake-oxlint"
  :after eglot
  :hook
  (eglot-managed-mode . flymake-oxlint-enable)
  :custom
  (flymake-oxlint-prefer-json-diagnostics t))


(use-package add-node-modules-path
  :hook (prog-mode . my/add-node-modules-path-based-on-lock)
  :custom
  (add-node-modules-path-command "pnpm bin")
  :config
  (defun my/add-node-modules-path-based-on-lock ()
    (let* ((root (locate-dominating-file default-directory
                                         (lambda (dir)
                                           (or (file-exists-p (expand-file-name "bun.lockb" dir))
                                               (file-exists-p (expand-file-name "bun.lock" dir))
                                               (file-exists-p (expand-file-name "pnpm-lock.yaml" dir))
                                               (file-exists-p (expand-file-name "package-lock.json" dir))))))
           (cmd (cond
                 ((or (file-exists-p (expand-file-name "bun.lockb" root))
                      (file-exists-p (expand-file-name "bun.lock" root)))
                  "bun pm bin")
                 ((file-exists-p (expand-file-name "pnpm-lock.yaml" root))
                  "pnpm bin")
                 ((file-exists-p (expand-file-name "package-lock.json" root))
                  "npm bin")
                 (t "pnpm bin"))))
      (setq-local add-node-modules-path-command cmd))
    (add-node-modules-path)))


(provide 'init-flymake)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flymake.el ends here
