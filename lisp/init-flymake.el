;; init-flymake.el --- Initialize flymake configurations.	-*- lexical-binding: t -*-


;;; Commentary:
;; some configuration of flymake.

;;; Code:


(use-package flymake
  :diminish
  :hook
  (prog-mode . flymake-mode)
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


;; (use-package flymake-eslint
;;   :custom
;;   (flymake-eslint-executable-name         "npx")
;;   (flymake-eslint-prefer-json-diagnostics t))


;; (use-package flymake-eslint
;;   :preface
;;   (defun me/flymake-eslint-enable-maybe ()
;;     "Enable `flymake-eslint' based on the project configuration.
;; Search for the project ESLint configuration to determine whether the buffer
;; should be checked."
;;     (when-let* ((root (locate-dominating-file (buffer-file-name) "package.json"))
;;                 (rc (locate-file ".eslintrc" (list root) '(".js" ".json"))))
;;       (make-local-variable 'exec-path)
;;       (push (file-name-concat root "node_modules" ".bin") exec-path)
;;       (flymake-eslint-enable))))


(provide 'init-flymake)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flymake.el ends here
