;;; init-rust --- Summary

;;; Commentary:
;; some configuration of rust.

;;; Code:


(use-package rustic
  :custom
  (rust-indent-offset          2)
  (rustic-rustfmt-config-alist '((tab_spaces . 2) (edition . "2021")))
  (rustic-format-trigger       'on-save)
  :config
  (defun rustic-cargo-test-run-with-args (args)
    "Start compilation process for  \"cargo test\" with ARGS."
    (interactive "sPlease enter test arguments: ")
    (setq rustic-test-arguments args)
    (rustic-cargo-test-run args)))


(use-package rust-playground)


(provide 'init-rust)

;;; init-rust.el ends here
