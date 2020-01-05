
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
;; custom-file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

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
  (require 'init-ibuffer)
  (require 'init-symbol-overlay)
  (require 'init-treemacs)
  (require 'init-shell)
  (require 'init-which-key)

  (require 'init-window)
  (require 'init-ivy)
  (require 'init-company)
  (require 'init-dired)

  ;; Programming
  (require 'init-flycheck)
  (require 'init-lsp)
  (require 'init-prog)
  (require 'init-go)
  (require 'init-lisp)

  ;; Web
  (require 'init-js)
  (require 'init-web)
)

