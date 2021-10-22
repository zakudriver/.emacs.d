;;; init-js --- Summary

;;; Commentary:
;; some configuration of js.

;;; Code:


(use-package js2-mode
  :mode
  (("\\.js$" . js2-mode))
  :custom
  (js-indent-level 2)
  (js2-basic-offset 2)
  (js-chain-indent t)
  (js2-highlight-level 3)
  (js2-mode-show-parse-errors t)
  (js2-mode-show-strict-warnings nil)
  (js2-strict-missing-semi-warning nil))


;; (use-package js-mode
;; :mode
;; (("\\.tsx$" . js-jsx-mode)
;;  ("\\.jsx$" . js-jsx-mode))
;; :custom
;; ((js-jsx-syntax   t)
;;  (js-indent-level 2)))


(use-package typescript-mode
  :custom
  (typescript-indent-level 2))


(use-package tide
  ;; :hook
  ;; ((typescript-mode web-mode js2-mode js-mode) . tide-setup)
  :bind
  (:map tide-mode-map
        ([remap evil-goto-definition] . tide-jump-to-definition)
        ([remap pop-tag-mark] . tide-jump-back)))
;; :config
;; (flycheck-add-next-checker 'jsx-tide '(warning . javascript-eslint) 'append)
;; (flycheck-add-next-checker 'tsx-tide '(warning . javascript-eslint) 'append)


(use-package ng2-mode
  :mode
  (".+\\.component\\.html$" . ng2-mode)
  ("\\.component.ts\\'" . typescript-mode)
  ("\\.service.ts\\'" . typescript-mode)
  ("\\.pipe.ts\\'" . typescript-mode)
  ("\\.directive.ts\\'" . typescript-mode)
  ("\\.guard.ts\\'" . typescript-mode)
  ("\\.module.ts\\'" . typescript-mode))


(use-package prettier-js
  :hook
  ((web-mode js2-mode ng2-mode typescript-mode scss-mode css-mode json-mode html-mode ng2-html-mode) . prettier-js-mode))
;; (web-mode . (lambda ()
;;               (unless (member (file-name-extension buffer-file-name) '("ejs"))
;;                 (prettier-js-mode))))
;; :custom
;; (prettier-js-args '("--single-quote" "true")) ;; "--print-width" "120")


(provide 'init-js)

;;; init-js.el ends here
