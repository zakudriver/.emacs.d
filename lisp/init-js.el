;;; init-js --- Summary

;;; Commentary:
;; some configuration of js.

;;; Code:


;; (use-package js2-mode
;;   :mode
;;   (("\\.js$" . js2-mode))
;;   :custom
;;   (js-indent-level 2)
;;   (js2-basic-offset 2)
;;   (js-chain-indent t)
;;   (js2-highlight-level 3)
;;   (js2-mode-show-parse-errors t)
;;   (js2-mode-show-strict-warnings nil)
;;   (js2-strict-missing-semi-warning nil))


(use-package js-mode
  :ensure nil
  ;; :mode
  ;; ("\\.tsx$" . js-jsx-mode)
  ;; ("\\.jsx$" . js-jsx-mode)
  :custom
  (js-jsx-syntax       t)
  (js-indent-level     2)
  (js-jsx-indent-level 2))


(define-derived-mode typescriptreact-mode web-mode "TypescriptReact"
  "A major mode for tsx.")


(use-package typescript-mode
  :mode
  ("\\.tsx\\'" . typescriptreact-mode)
  :custom
  (typescript-indent-level 2))


;; (use-package tide
;;   ;; :hook
;;   ;; ((typescript-mode web-mode js2-mode js-mode) . tide-setup)
;;   :bind
;;   (:map tide-mode-map
;;         ([remap evil-goto-definition] . tide-jump-to-definition)
;;         ([remap pop-tag-mark]         . tide-jump-back)))
;; :config
;; (flycheck-add-next-checker 'jsx-tide '(warning . javascript-eslint) 'append)
;; (flycheck-add-next-checker 'tsx-tide '(warning . javascript-eslint) 'append)


(use-package ng2-mode
  :mode
  (".+\\.component\\.html$" . ng2-mode)
  ("\\.component.ts\\'"     . ng2-mode)
  ;; ("\\.service.ts\\'"       . ng2-mode)
  ;; ("\\.pipe.ts\\'"          . ng2-mode)
  ;; ("\\.directive.ts\\'"     . ng2-mode)
  ;; ("\\.guard.ts\\'"         . ng2-mode)
  ;; ("\\.module.ts\\'"        . ng2-mode)
  )


(provide 'init-js)

;;; init-js.el ends here
