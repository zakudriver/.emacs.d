;;; Code:

(use-package flycheck
  :diminish
  :hook (after-init . global-flycheck-mode)
  :init
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side . bottom)
                 (reusable-frames . visible)
                 (window-height . 0.2)))
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  ;; Only check while saving and opening files
  (flycheck-check-syntax-automatically '(save mode-enabled))
  ;; Set fringe style
  (flycheck-indication-mode 'right-fringe)
  :config
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))

  ;; (use-package flycheck-color-mode-line
  ;;   :hook (flycheck-mode . flycheck-color-mode-line-mode))
  ;; Display Flycheck errors in GUI tooltips
  (if (display-graphic-p)
      (use-package flycheck-posframe
        :custom-face (flycheck-posframe-border-face ((t (:inherit default))))
        :hook (flycheck-mode . flycheck-posframe-mode)
        :init (setq flycheck-posframe-border-width 1
                    flycheck-posframe-inhibit-functions
                    '((lambda (&rest _) (bound-and-true-p company-backend)))))
    (use-package flycheck-popup-tip
      :hook (flycheck-mode . flycheck-popup-tip-mode))))



(provide 'init-flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flycheck.el ends here
