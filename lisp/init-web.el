;;; Code:


;; CSS mode
(use-package css-mode
  :custom
  (css-indent-offset 2))

;; SCSS mode
(use-package scss-mode
  :custom
  ;; Disable complilation on save
  (scss-compile-at-save nil))


;; major-mode for editing multiple web formats
(use-package web-mode
  :mode
  ((".*[^\\(\\.component\\)]\\.html$" . web-mode)
   ("\\.jsx$" . web-mode)
   ("\\.tsx$" . web-mode)
   ("\\.ejs$" . web-mode))
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
  (web-mode-enable-comment-interpolation t))


;; emmet-mode: dynamic snippets for HTML
(use-package emmet-mode
  :hook
  ((web-mode ng2-html-mode) . emmet-mode)
  :bind
  ((:map emmet-mode-keymap
         ("C-. [" . emmet-prev-edit-point)
         ("C-. ]" . emmet-next-edit-point)
         ("<backtab>" . emmet-expand-yas)))
  :custom
  (emmet-move-cursor-between-quotes t)
  (emmet-indentation 2)
  (emmet-expand-jsx-className? t))


(provide 'init-web)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-web.el ends here
