;;; Code:



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; javascript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; js2-mode: enhanced JavaScript editing mode
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :mode (("\\.js$" . js2-mode))
  :hook ((js2-mode . js-tide-setup-hook))
  :config
  ;; have 2 space indentation by default
  (setq js-indent-level 2
        js2-basic-offset 2
        js-chain-indent t)

  ;; use eslint_d insetad of eslint for faster linting
  (setq flycheck-javascript-eslint-executable "eslint_d")

  ;; Try to highlight most ECMA built-ins
  (setq js2-highlight-level 3)

  ;; turn off all warnings in js2-mode
  (setq js2-mode-show-parse-errors t)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-strict-missing-semi-warning nil)

  (defun js-tide-setup-hook ()
    ;; configure tide
    (tide-setup)
    ;; highlight identifiers
    (tide-hl-identifier-mode +1)
    ;;enable eldoc-mode
    (eldoc-mode)
    ;; enable flycheck
    (flycheck-mode)
    ;; format javascript files using prettier
    (prettier-js-mode)

    (company-mode)

    (add-node-modules-path)

    ;; company-backends setup
    (set (make-local-variable 'company-backends)
         '((company-tide company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev)))

    ;; configure javascript-tide checker to run after your default javascript checker
    (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; typescript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; typescript: major mode for editing typescript files
;; https://github.com/ananthakumaran/typescript.el
(use-package typescript-mode
  :hook ((typescript-mode . ts-tide-setup-hook))
  :bind ((:map typescript-mode-map
               ("C-c C-t" . tide-documentation-at-point)
               ("C-c T p" . typescript/open-region-in-playground)))
  :config
  (defun ts-tide-setup-hook ()
    ;; configure tide
    (tide-setup)
    ;; highlight identifiers
    (tide-hl-identifier-mode +1)
    ;;enable eldoc-mode
    (eldoc-mode)
    ;; enable flycheck
    (flycheck-mode)
    ;; format typescript files using prettier
    (prettier-js-mode)

    (add-node-modules-path)

    (company-mode)

    ;; company-backends setup
    (set (make-local-variable 'company-backends)
         '((company-tide company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))

  (defun typescript/open-region-in-playground (start end)
    "Open selected region in http://www.typescriptlang.org/Playground
                 If nothing is selected - open the whole current buffer."
    (interactive (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list (point-min) (point-max))))
    (browse-url (concat "http://www.typescriptlang.org/Playground#src="
                        (url-hexify-string (buffer-substring-no-properties start end))))))


;; tide: TypeScript Interactive Development Environment for Emacs
;; https://github.com/ananthakumaran/tide
(use-package tide
  :after typescript-mode
  :config
  ;; use 2 space indentation
  (setq typescript-indent-level 2)

  ;; add tslint checker for flycheck
  (flycheck-add-next-checker 'typescript-tide
                             'typescript-tslint)

  (setq tide-completion-detailed t)

  (add-hook 'tide-mode-hook
            (lambda ()
              (add-hook 'kill-buffer-hook #'+javascript|cleanup-tide-processes nil t))))


;;;###autoload
(defun +javascript|cleanup-tide-processes ()
  "Clean up dangling tsserver processes if there are no more buffers with
`tide-mode' active that belong to that server's project."
  (when tide-mode
    (unless (cl-loop with project-name = (tide-project-name)
                     for buf in (delq (current-buffer) (buffer-list))
                     if (and (buffer-local-value 'tide-mode buf)
                             (with-current-buffer buf
                               (string= (tide-project-name) project-name)))
                     return buf)
      (kill-process (tide-current-server)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package prettier-js
  :hook ((js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)))


;; add-node-modules-path
;;(use-package add-node-modules-path
;;  :init (add-hook 'js2-mode-hook #'add-node-modules-path)
;;  (add-hook 'typescript-mode-hook #'add-node-modules-path))


(provide 'init-js)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-js.el ends here
