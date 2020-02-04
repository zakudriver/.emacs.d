;;; Code:

;; (setq debug-on-error t)

(when (version< emacs-version "26.1")
  (error "This requires Emacs 26.1 and above!"))


;; Speed up startup
(defvar centaur-gc-cons-threshold (if (display-graphic-p) 8000000 800000)
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

(defvar centaur-gc-cons-upper-limit (if (display-graphic-p) 400000000 100000000)
  "The temporary value for `gc-cons-threshold' to defer it.")

(defvar centaur-gc-timer (run-with-idle-timer 10 t #'garbage-collect)
  "Run garbarge collection when idle 10s.")

(defvar default-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)
(setq gc-cons-threshold centaur-gc-cons-upper-limit)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after startup."
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold centaur-gc-cons-threshold)

            ;; GC automatically while unfocusing the frame
            ;; `focus-out-hook' is obsolete since 27.1
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                  (lambda ()
                    (unless (frame-focus-state)
                      (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))

            (defun my-minibuffer-setup-hook ()
              (setq gc-cons-threshold centaur-gc-cons-upper-limit))

            (defun my-minibuffer-exit-hook ()
              (setq gc-cons-threshold centaur-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)))


;; Load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; Load custom theme file
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

;; benchmarking
(require 'init-benchmarking)
;; custom-file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; set my own configuration
(with-temp-message ""
  ;; Constants
  (require 'init-const)

  ;; Customization
  (require 'init-custom)

  ;; Packages
  (require 'init-package)

  ;; Preferences
  (require 'init-base)
  (require 'init-funcs)
  (require 'init-utils)
  (require 'init-evil)
  (require 'init-modeline)
  (require 'init-ui)

  (require 'init-edit)
  (require 'init-highlight)
  (require 'init-ibuffer)
  (require 'init-treemacs)
  (require 'init-shell)
  (require 'init-which-key)

  (require 'init-window)
  (require 'init-ivy)
  (require 'init-company)
  (require 'init-dired)
  (require 'init-flycheck)
  (require 'init-yasnippet)
  (require 'init-projectile)

  ;; Programming
  (require 'init-lsp)
  (require 'init-prog)
  (require 'init-org)
  (require 'init-markdown)
  (require 'init-go)
  (require 'init-cpp)
  (require 'init-lisp)

  ;; Web
  (require 'init-js)
  (require 'init-web)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
