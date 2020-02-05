;;; Code:


(setq-default c-basic-offset 2)


(use-package modern-cpp-font-lock
  :diminish
  :hook
  (c++-mode . modern-c++-font-lock-mode))


(use-package clang-format
  :diminish
  :bind
  (:map c-mode-base-map
        ("C-x f" . clang-format-buffer))
  :custom
  (clang-format-style-option "llvm")
  :hook
  (c++-mode . (lambda ()
                     (add-hook (make-local-variable 'before-save-hook)
                               'clang-format-buffer))))

;;; C/C++ headers completion
(use-package company-c-headers
  :defer t
  :hook
  (c++-mode . (lambda () (add-to-list 'company-backends 'company-c-headers))))


(provide 'init-cpp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-cpp.el ends here
