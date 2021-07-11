;;; Code:


(eval-when-compile
  (require 'init-const))


(use-package flycheck
  :hook
  (after-init . global-flycheck-mode)
  :init
  (add-to-list 'display-buffer-alist
               `(,(eval `(rx bos ,kumo/flycheck-errors-buffer-name eos))
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (reusable-frames . visible)
                 (window-height . 0.2)))
  :custom
  (flycheck-global-modes
   '(not text-mode outline-mode fundamental-mode lisp-interaction-mode
         org-mode diff-mode shell-mode eshell-mode term-mode vterm-mode))
  (flycheck-emacs-lisp-load-path 'inherit)
  ;; Only check while saving and opening files
  (flycheck-check-syntax-automatically '(save mode-enabled))
  ;; Set fringe style
  (flycheck-indication-mode 'right-fringe)
  :config
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))

  (use-package flycheck-popup-tip
    :hook
    (flycheck-mode . flycheck-popup-tip-mode)
    :custom
    (flycheck-popup-tip-error-prefix " \u2717 ")))


(provide 'init-flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flycheck.el ends here
