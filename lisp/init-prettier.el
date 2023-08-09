;;; init-prettier --- Summary

;;; Commentary:
;; some configuration of prettier code.

;;; Code:

(use-package prettier-js
  :hook
  ((web-mode typescriptreact-mode js-mode ng2-mode typescript-mode scss-mode css-mode json-mode html-mode ng2-html-mode graphql-mode yaml-mode) . prettier-js-mode))
;; (web-mode . (lambda ()
;;               (unless (member (file-name-extension buffer-file-name) '("ejs"))
;;                 (prettier-js-mode))))
;; :custom
;; (prettier-js-args '("--single-quote" "true")) ;; "--print-width" "120")


(use-package clang-format+
  :hook
  ((c-mode c++-mode protobuf-mode) . clang-format+-mode))


(provide 'init-prettier)

;;; init-prettier.el ends here
