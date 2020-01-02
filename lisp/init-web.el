;;; Code:


(defun web-lsp-html-setup ()
  "Function to setup `lsp-html'"
  (lsp)
  (emmet-mode)

  (set (make-local-variable 'company-backends)
       '((company-lsp company-files :with company-yasnippet)
         (company-dabbrev-code company-dabbrev)))

  (setq-local lsp-highlight-symbol-at-point nil))


(defun web-tsx-setup-hook ()
  ;; (flycheck-mode)
  (prettier-js-mode)

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
         ("\\.mustache\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.hbs\\'" . web-mode))
  :hook
  ((web-mode . company-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-html-entities-fontification t
        web-mode-auto-close-style 2)
  (setq web-mode-enable-auto-quoting nil
        web-mode-enable-auto-pairing nil)


  ;; highlight matching tag
  (setq web-mode-enable-current-element-highlight t)
  (add-hook 'web-mode-hook
            (lambda ()
              (pcase (file-name-extension buffer-file-name)
                ("tsx" (web-tsx-setup-hook))
                ("html" (web-lsp-html-setup)))))

  (setq web-mode-enable-css-colorization t))


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
