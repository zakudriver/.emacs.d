;;; init-swift --- Summary

;;; Commentary:
;; some configuration of swift.

;;; Code:


(use-package swift-mode)


(use-package lsp-sourcekit
  :after lsp-mode
  :custom
  (lsp-sourcekit-executable (string-trim (shell-command-to-string "xcrun --find sourcekit-lsp"))))


(use-package flycheck-swift
  :after flycheck
  :hook
  (swift-mode . flycheck-swift-setup))


(provide 'init-swift)

;;; init-swift.el ends here
