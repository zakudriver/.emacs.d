
;; (setq debug-on-error t)

(when (version< emacs-version "26.1")
  (error "This requires Emacs 26.1 and above!"))

;; Optimize loading performance
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 40000000)

(add-hook #'emacs-startup-hook
          (lambda ()
            "Restore defalut values after init"
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 800000)
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook #'focus-out-hokk #'garbage-collect))

            (defun my-minibuffer-setup-hook ()
              (setq gc-cons-threshold 40000000))

            (defun my-minibuffer-exit-hook ()
              (setq gc-cons-threshold 800000))

            (add-hook #'minibuffer-setup-hook #'my-minibuffer-setup-hook)
            (add-hook #'minibuffer-exit-hook #'my-minibuffer-exit-hook)))

;; Load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; benchmarking
(require 'init-benchmarking)

;; set my own configuration
(with-temp-message ""                   ;抹掉插件启动的输出
  ;; Constants
  (require 'init-const)

  ;; Customization
  (require 'init-custom)

  ;; Packages
  ;; Without this comment Emacs25 adds (package-initialize) here
  ;; (package-initialize)
  (require 'init-package)

  ;; Preferences
  (require 'init-basic)
  (require 'init-funcs)
  (require 'init-utils)
  (require 'init-evil)
  (require 'init-ui)
  (require 'init-edit)
  (require 'init-symbol-overlay)
  (require 'init-treemacs)
  (require 'init-shell)
  (require 'init-which-key)

  (require 'init-window)
  (require 'init-ivy)
  (require 'init-company)
  (require 'init-dired)

  ;; Programming
  (require 'init-lsp)
  (require 'init-prog)
  (require 'init-go)
  (require 'init-lisp)

  ;; Web
  (require 'init-js)
  (require 'init-web)

)

(put 'magit-diff-edit-hunk-commit 'disabled nil)

;; Title
(setq frame-title-format
      '("Emacs " emacs-version "@" user-login-name " : "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(setq icon-title-format frame-title-format)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("5e515425f8a5ce00097d707742eb5eee09b27cebc693b8998c734305cbdce1f5" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(lsp-auto-guess-root t t)
 '(lsp-enable-snippet nil t)
 '(lsp-prefer-flymake nil t)
 '(lsp-print-io nil t)
 '(lsp-print-performance nil t)
 '(lsp-trace nil t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-dired-dir-face ((t (:foreground nil)))))
