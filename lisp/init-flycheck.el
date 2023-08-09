;;; init-flycheck --- Summary

;;; Commentary:
;; some configuration of flycheck.

;;; Code:


(eval-when-compile
  (require 'init-const)
  (require 'init-custom))


(use-package flycheck
  :commands flycheck-list-errors
  :autoload flycheck-redefine-standard-error-levels
  :hook
  (prog-mode . global-flycheck-mode)
  ;; (tide-mode  . use-eslint-from-nodemodules)
  ;; (web-mode . use-eslint-tide)
  :bind
  (:map flycheck-mode-map
        ("C-c c c" . my-flycheck-list-errors-toggle)
        ("C-c c s" . my-flycheck-list-errors-select-window))
  :init
  (add-to-list 'display-buffer-alist
               `(,(eval `(rx bos ,my/flycheck-errors-buffer-name eos))
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.15)))
  :custom
  (flycheck-global-modes my/flycheck-boot-mode)
  (flycheck-emacs-lisp-load-path 'inherit)
  ;; Only check while saving and opening files
  (flycheck-check-syntax-automatically '(save mode-enabled))
  ;; Set fringe style
  (flycheck-indication-mode 'right-fringe)
  :config
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))

  (flycheck-redefine-standard-error-levels "⏴" 'flycheck-fringe-bitmap-arrow)
  
  (defun my-flycheck-list-errors-toggle ()
    "Open or delete flycheck-errors-list window."
    (interactive)
    (let ((w (get-buffer-window my/flycheck-errors-buffer-name)))
      (if w
          (delete-window w)
        (flycheck-list-errors)))))


;; (use-package flycheck-popup-tip
;;   :hook
;;   (flycheck-mode . flycheck-popup-tip-mode))



(provide 'init-flycheck)

;;; init-flycheck.el ends here
