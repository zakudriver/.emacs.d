;;; init-flycheck --- Summary

;;; Commentary:
;; some configuration of flycheck.

;;; Code:


(eval-when-compile
  (require 'init-const)
  (require 'init-custom))


(use-package flycheck
  :commands flycheck-list-errors
  :hook
  (prog-mode . global-flycheck-mode)
  ;; (tide-mode  . use-eslint-from-nodemodules)
  ;; (web-mode . use-eslint-tide)
  :bind
  (:map flycheck-mode-map
        ("C-c c c" . kumo-flycheck-list-errors-toggle)
        ("C-c c s" . kumo-flycheck-list-errors-select-window))
  :init
  (add-to-list 'display-buffer-alist
               `(,(eval `(rx bos ,kumo/flycheck-errors-buffer-name eos))
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.15)))
  :custom
  (flycheck-global-modes kumo/flycheck-boot-mode)
  (flycheck-emacs-lisp-load-path 'inherit)
  ;; Only check while saving and opening files
  (flycheck-check-syntax-automatically '(save mode-enabled))
  ;; Set fringe style
  (flycheck-indication-mode 'right-fringe)
  :config
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))

  (defun kumo-flycheck-list-errors-toggle ()
    "Open or delete flycheck-errors-list window."
    (interactive)
    (let ((w (get-buffer-window kumo/flycheck-errors-buffer-name)))
      (if w
          (delete-window w)
        (flycheck-list-errors)))))


;; (use-package flycheck-popup-tip
;;   :hook
;;   (flycheck-mode . flycheck-popup-tip-mode)
;;   :custom
;;   (flycheck-popup-tip-error-prefix " \u2717 "))

;; (defun use-eslint-from-nodemodules ()
;;   (let* ((root (locate-dominating-file
;;                 (or (buffer-file-name) default-directory)
;;                 "node_modules"))
;;          (eslint
;;           (and root
;;                (expand-file-name "node_modules/.bin/eslint"
;;                                  root))))
;;     (when (and eslint (file-executable-p eslint))
;;       (setq-local flycheck-javascript-eslint-executable eslint)
;;       ;; (flycheck-select-checker 'javascript-eslint)
;;       (flycheck-add-next-checker 'tsx-tide 'javascript-eslint 'append)
;;       (flycheck-add-next-checker 'typescript-tide 'javascript-eslint 'append))))

;; (flycheck-add-mode 'javascript-eslint 'web-mode)
;; (flycheck-add-mode 'javascript-eslint 'typescript-mode)


(provide 'init-flycheck)

;;; init-flycheck.el ends here
