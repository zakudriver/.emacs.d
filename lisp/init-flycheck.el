;; Flycheck configurations.
;;
;;; Code:

(use-package flycheck
  :diminish flycheck-mode
  :config
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Display Flycheck errors in GUI tooltips
  (if (display-graphic-p)
      (use-package flycheck-pos-tip
        :hook (after-init . flycheck-pos-tip-mode)
        :config (setq flycheck-pos-tip-timeout 30))
    (use-package flycheck-popup-tip
      :hook (after-init . flycheck-popup-tip-mode)))

  ;; Jump to and fix syntax errors via `avy'
  (use-package avy-flycheck
    :hook (after-init . avy-flycheck-setup)))

(provide 'init-flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flycheck.el ends here
