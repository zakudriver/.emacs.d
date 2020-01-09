;;; Code:


;; major mode for editing js files
;; (use-package js2-mode
;;   :mode (("\\.js$" . js2-mode))
;;   :config
;;   (setq js-indent-level 2
;;         js2-basic-offset 2
;;         js-chain-indent t)
;;   (setq js2-highlight-level 3)
;;   (setq js2-mode-show-parse-errors t)
;;   (setq js2-mode-show-strict-warnings nil)
;;   (setq js2-strict-missing-semi-warning nil))


;; major mode for editing ts files
(use-package typescript-mode
  :config
  (setq typescript-indent-level 2))


;; major mode for typescript language server
(use-package tide
  :hook
  (typescript-mode . tide-setup)
  (typescript-mode . tide-hl-identifier-mode)
  :bind
  (:map tide-mode-map
        ([remap evil-goto-definition] . tide-jump-to-definition)
        ([remap pop-tag-mark] . tide-jump-back))
)


;; major mode for editing *.component.html files
(use-package ng2-mode
  :mode
  (".+\\.component\\.html$" . ng2-mode))


;; format
(use-package prettier-js
  :init
  (setq prettier-js-args '("--single-quote" "true" "--print-width" "120"))
  :hook
  (js2-mode . prettier-js-mode)
  (typescript-mode . prettier-js-mode)
  (web-mode . prettier-js-mode))




(provide 'init-js)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-js.el ends here
