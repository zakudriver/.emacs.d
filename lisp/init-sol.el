;;; init-sol --- Summary

;;; Commentary:
;; some configuration of solidity.

;;; Code:


(use-package solidity-mode)


(use-package solidity-flycheck
  :after flycheck
  :hook
  (solidity-mode . solidity-flycheck))


(use-package company-solidity)


(provide 'init-sol)

;;; init-sol.el ends here
