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
  (sideline-backends-right       '(sideline-flymake)))


(use-package add-node-modules-path
  :hook
  (prog-mode . (lambda ()
                 (when (apply 'derived-mode-p my/eslint-enable-mode)
                   (my/add-node-modules-path-based-on-lock))))
  :custom
  (add-node-modules-path-command '("bun pm bin"))
  ;; (add-node-modules-path-debug   t)
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
                  "pnpm bin -w")
                 ((file-exists-p (expand-file-name "package-lock.json" root))
                  "npm bin")
                 (t "npm bin"))))
      (setq-local add-node-modules-path-command (list cmd)))
    (add-node-modules-path)))


(use-package flymake-eslint
  :custom
  (flymake-eslint-defer-binary-check      t)
  (flymake-eslint-prefer-json-diagnostics t)
  :hook
  (eglot-managed-mode . (lambda ()
                          (when (apply 'derived-mode-p my/eslint-enable-mode)
                            (flymake-eslint-enable)))))


(use-package flymake-oxlint
  :load-path "~/.emacs.d/site-lisp/flymake-oxlint"
  :demand t
  :hook
  (eglot-managed-mode . (lambda ()
                          (when (apply 'derived-mode-p my/eslint-enable-mode)
                            (flymake-oxlint-enable))))
  :custom
  (flymake-oxlint-defer-binary-check      t)
  (flymake-oxlint-prefer-json-diagnostics t))


(provide 'init-flymake)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flymake.el ends here
