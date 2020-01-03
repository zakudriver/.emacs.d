;;; Code:


;; CSS mode
(use-package css-mode
  :ensure nil
  :init (setq css-indent-offset 2))

;; SCSS mode
;; (use-package scss-mode
;;   :init
;;   ;; Disable complilation on save
;;   (setq scss-compile-at-save nil))


(defun web-lsp-html-setup ()
  (lsp)
  (emmet-mode)

  (set (make-local-variable 'company-backends)
       '((company-lsp company-files :with company-yasnippet)
         (company-dabbrev-code company-dabbrev)))

  (setq-local lsp-highlight-symbol-at-point nil))


(defun web-tsx-setup-hook ()
  ;; company-backends setup
  (set (make-local-variable 'company-backends)
       '((company-dabbrev-code company-dabbrev)))
  ;; enable typescript-tslint checker
  ;; (flycheck-add-mode 'typescript-tslint 'web-mode)
)

;; web-mode: major-mode for editing multiple web formats
(use-package web-mode
  :mode
  (("\\.html$" . web-mode)
         ("\\.djhtml$" . web-mode)
         ("\\.tsx$" . web-mode)
         ("\\.jsx$" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.hbs\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-html-entities-fontification t)
  (web-mode-auto-close-style 2)
  (web-mode-enable-auto-quoting nil)
  (web-mode-enable-auto-pairing nil)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-css-colorization t)
  ;; :config
  ;; (add-hook 'web-mode-hook
  ;;           (lambda ()
  ;;             (pcase (file-name-extension buffer-file-name)
  ;;               ("tsx" (web-tsx-setup-hook))
  ;;               )))
)


;; emmet-mode: dynamic snippets for HTML
(use-package emmet-mode
  :bind
  ((:map emmet-mode-keymap
         ("C-c [" . emmet-prev-edit-point)
         ("C-c ]" . emmet-next-edit-point)))
  :config
  (setq emmet-move-cursor-between-quotes t)
  (setq emmet-indentation 2)
  (unbind-key "C-M-<left>" emmet-mode-keymap)
  (unbind-key "C-M-<right>" emmet-mode-keymap))



(provide 'init-web)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-web.el ends here
