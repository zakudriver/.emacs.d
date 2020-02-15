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


(defun tsx-setup-tide-mode ()
  "Setup tide mode when tsx file."
  (when (string-equal "tsx" (file-name-extension buffer-file-name))
    (tide-setup)
    (tide-hl-identifier-mode)))


;; major-mode for editing multiple web formats
(use-package web-mode
  :mode
  ((".*[^\\(\\.component\\)]\\.html$" . web-mode)
   ("\\.tsx$" . web-mode)
   ("\\.ejs$" . web-mode))
  :hook
  (web-mode . tsx-setup-tide-mode)
  :bind
  (:map web-mode-map
        ("C-/" . web-mode-comment-or-uncomment))
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
  (web-mode-comment-style 2)
  (web-mode-enable-comment-annotation t)
  ;; :config
  ;; (add-to-list 'web-mode-comment-formats '("tsx" . "//"))
  ;; (add-to-list 'web-mode-comment-formats '("jsx" . "//"))
)


;; emmet-mode: dynamic snippets for HTML
(use-package emmet-mode
  :bind
  ((:map emmet-mode-keymap
         ("C-c [" . emmet-prev-edit-point)
         ("C-c ]" . emmet-next-edit-point)
         ("<backtab>" . emmet-expand-yas)))
  :hook
  ((web-mode ng2-html-mode) . emmet-mode)
  :custom
  (emmet-move-cursor-between-quotes t)
  (emmet-indentation 2)
  (emmet-expand-jsx-className? t))



(provide 'init-web)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-web.el ends here
