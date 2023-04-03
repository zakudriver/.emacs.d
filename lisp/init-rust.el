;;; init-rust --- Summary

;;; Commentary:
;; some configuration of rust.

;;; Code:


(use-package rustic
  :custom
  (rust-indent-offset          2)
  (rustic-rustfmt-config-alist '((tab_spaces . 2)))
  (rustic-format-trigger       'on-save))


(use-package rust-playground)


(provide 'init-rust)

;;; init-rust.el ends here
