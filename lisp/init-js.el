;;; Code:


;; major mode for editing js files
(use-package js2-mode
  :mode (("\\.js$" . js2-mode))
  :custom
  (js-indent-level 2)
  (js2-basic-offset 2)
  (js-chain-indent t)
  (js2-highlight-level 3)
  (js2-mode-show-parse-errors t)
  (js2-mode-show-strict-warnings nil)
  (js2-strict-missing-semi-warning nil))


;; major mode for editing ts files
(use-package typescript-mode
  :custom
  (typescript-indent-level 2))


;; major mode for typescript language server
(use-package tide
  :hook
  ((typescript-mode js2-mode) . tide-setup)
  ((typescript-mode js2-mode) . tide-hl-identifier-mode)
  (web-mode . (lambda ()
                (when (string= "tsx" (file-name-extension buffer-file-name))
                  (tide-setup)
                  (tide-hl-identifier-mode))))
  :bind
  (:map tide-mode-map
        ([remap evil-goto-definition] . tide-jump-to-definition)
        ([remap pop-tag-mark] . tide-jump-back)))


;; major mode for editing *.component.html files
(use-package ng2-mode
  :mode
  (".+\\.component\\.html$" . ng2-mode)
  ("\\.component.ts\\'" . typescript-mode)
  ("\\.service.ts\\'" . typescript-mode)
  ("\\.pipe.ts\\'" . typescript-mode)
  ("\\.directive.ts\\'" . typescript-mode)
  ("\\.guard.ts\\'" . typescript-mode)
  ("\\.module.ts\\'" . typescript-mode))


;; format
(use-package prettier-js
  :hook
  ((js2-mode ng2-mode typescript-mode scss-mode) . prettier-js-mode)
  (web-mode . (lambda ()
                (when (not (member (file-name-extension buffer-file-name) '("ejs")))
                  (prettier-js-mode))))
  :custom
  (prettier-js-args '("--single-quote" "true" "--print-width" "120")))



(provide 'init-js)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-js.el ends here
