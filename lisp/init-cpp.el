;;; Code:


;; cpp keyword highlight
(use-package modern-cpp-font-lock
  :ensure nil
  :hook
  (c++-mode . modern-c++-font-lock-mode))


(use-package clang-format
  :ensure nil
  :bind
  (:map c-mode-map 
        ("C-. f" . clang-format-buffer)
        :map c++-mode-map
        ("C-. f" . clang-format-buffer))
  :custom
  (clang-format-style-option "llvm")
  :hook
  ((c-mode c++-mode) . (lambda ()
                         (add-hook (make-local-variable 'before-save-hook)
                                   'clang-format-buffer))))


(use-package irony
  :ensure nil
  :hook
  ((c-mode c++-mode) . irony-mode)
  :bind
  (:map irony-mode-map
        ([remap completion-at-point] . counsel-irony)
        ([remap complete-symbol] . counsel-irony))
  :config
  (irony-cdb-autosetup-compile-options))


(use-package flycheck-irony
  :ensure nil
  :after flycheck
  :hook
  (c-mode . flycheck-irony-setup)
  (c++-mode . (lambda ()
                (flycheck-irony-setup)
                (setq flycheck-clang-language-standard "c++11")
                (setq irony-additional-clang-options '("-std=c++11")))))


;; C/C++ headers completion
(use-package company-c-headers
  :ensure nil
  :hook
  ((c-mode c++-mode) . (lambda () (add-to-list 'company-backends 'company-c-headers))))


;; backends for irony
(use-package company-irony
  :ensure nil
  :hook
  ((c-mode c++-mode) . (lambda () (add-to-list 'company-backends 'company-irony))))


;; backends for irony-c-header
(use-package company-irony-c-headers
  :ensure nil
  :hook
  ((c-mode c++-mode) . (lambda () (add-to-list 'company-backends 'company-irony-c-headers))))


(provide 'init-cpp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-cpp.el ends here
