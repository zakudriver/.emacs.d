;;; Code:


;; js2-mode: enhanced JavaScript editing mode
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :mode (("\\.js$" . js2-mode))
  :hook ((js2-mode . (lambda ()
    (flycheck-mode)
    (prettier-js-mode)
    ;;(add-node-modules-path)
    )))
  :config
  ;; have 2 space indentation by default
  (setq js-indent-level 2
        js2-basic-offset 2
        js-chain-indent t)

  ;; use eslint_d insetad of eslint for faster linting
  (setq flycheck-javascript-eslint-executable "eslint_d")
  ;; Try to highlight most ECMA built-ins
  (setq js2-highlight-level 3)
  ;; turn off all warnings in js2-mode
  (setq js2-mode-show-parse-errors t)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-strict-missing-semi-warning nil))



;; typescript: major mode for editing typescript files
;; https://github.com/ananthakumaran/typescript.el
(use-package typescript-mode
  :hook ((typescript-mode . (lambda ()
    (flycheck-mode)
    (prettier-js-mode)
    ;;(add-node-modules-path)
    )))
  :config 
  (setq typescript-indent-level 2))



;; format
(use-package prettier-js
  :init
  (setq prettier-js-args '("--single-quote" "true" "--print-width" "120"))
  :hook
  ((js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)))



(provide 'init-js)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-js.el ends here
