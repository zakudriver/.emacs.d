;;; Code:

;; web-mode: major-mode for editing multiple web formats
;; http://web-mode.org/ , https://github.com/fxbois/web-mode
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

  (with-eval-after-load "smartparens"
    (defun +web-is-auto-close-style-3 (_id action _context)
      (and (eq action 'insert)
           (eq web-mode-auto-close-style 3)))
    (sp-local-pair 'web-mode "<" nil :unless '(:add +web-is-auto-close-style-3))

    ;; let smartparens handle these
    (setq web-mode-enable-auto-quoting nil
          web-mode-enable-auto-pairing t)

    ;; 1. Remove web-mode auto pairs whose end pair starts with a latter
    ;;    (truncated autopairs like <?p and hp ?>). Smartparens handles these
    ;;    better.
    ;; 2. Strips out extra closing pairs to prevent redundant characters
    ;;    inserted by smartparens.
    (dolist (alist web-mode-engines-auto-pairs)
      (setcdr alist
              (cl-loop for pair in (cdr alist)
                       unless (string-match-p "^[a-z-]" (cdr pair))
                       collect (cons (car pair)
                                     (string-trim-right (cdr pair) "\\(?:>\\|]\\|}\\)+")))))
    (setf (alist-get nil web-mode-engines-auto-pairs) nil))

  ;; highlight matching tag
  (setq web-mode-enable-current-element-highlight t)

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
         '((company-tide company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev)))
    ;; enable typescript-tslint checker
    ;; (flycheck-add-mode 'typescript-tslint 'web-mode)
  )
  

  (add-hook 'web-mode-hook
            (lambda ()
              (pcase (file-name-extension buffer-file-name)
                ("tsx" (web-tsx-setup-hook))
                ("html" (web-lsp-html-setup)))))

  ;; colorize colors in buffers
  (setq web-mode-enable-css-colorization t))


;; emmet-mode: dynamic snippets for HTML
;; https://github.com/smihica/emmet-mode
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


;; sass-mode
;; (use-package sass-mode
;;   :mode
;;   ("\\.sass$" . sass-mode))


(provide 'init-web)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-web.el ends here
