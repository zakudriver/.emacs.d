;;; Code:

;; (setq debug-on-error t)

(when (version< emacs-version "26.1")
  (error "This requires Emacs 26.1 and above!"))


;; Speed up startup
(defvar centaur-gc-cons-threshold (if (display-graphic-p) 8000000 800000)
  "The default value to use for `gc-cons-threshold'.
If you experience freezing, decrease this.
If you experience stuttering, increase this.")

(defvar centaur-gc-cons-upper-limit (if (display-graphic-p) 400000000 100000000)
  "The temporary value for `gc-cons-threshold' to defer it.")

(defvar centaur-gc-timer (run-with-idle-timer 10 t #'garbage-collect)
  "Run garbarge collection when idle 10s.")

(defvar default-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil
      gc-cons-threshold centaur-gc-cons-upper-limit)
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
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
;; (advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

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
  (require 'init-keys)
  ;; (require 'init-evil)
  ;; (require 'init-modeline)
  (require 'init-font)
  (require 'init-theme)
  (require 'init-ui)

  (require 'init-edit)
  (require 'init-highlight)
  (require 'init-ibuffer)

  (require 'init-window)
  (require 'init-ivy)
  (require 'init-company)
  (require 'init-dired)
  (require 'init-flycheck)
  (require 'init-yasnippet)
  (require 'init-projectile)

  ;; polymode
  (require 'init-polymode)

  ;; Programming
  (require 'init-lsp)
  (require 'init-prog)
  (require 'init-org)
  (require 'init-go)
  (require 'init-dart)
  (require 'init-cpp)
  (require 'init-ruby)
  (require 'init-lisp)

  ;; Web
  (require 'init-js)
  (require 'init-web)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-parentheses-background-colors '("#F4CCE1") nil nil "Customized with use-package highlight-parentheses")
 '(highlight-parentheses-colors '("#73317a") nil nil "Customized with use-package highlight-parentheses")
 '(safe-local-variable-values
   '((eval progn
           (require 'find-file-in-project)
           (setq ffip-prune-patterns
                 `("*/@querycap-canary/*" ,@ffip-prune-patterns)))
     (ffip-find-options . "-not -size +64k -not -iwholename '*/dist/*'")
     (ffip-project-root . "/Users/rx-78-kum0/code/rockontrol/webapp/"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
 '(org-pomodoro-keep-killed-pomodoro-time t)
 '(org-pomodoro-mode-line ((t (:inherit warning))))
 '(org-pomodoro-mode-line-break ((t (:inherit success))))
 '(org-pomodoro-mode-line-overtime ((t (:inherit error))))
 '(pulse-highlight-face ((t (:inherit region))))
 '(pulse-highlight-start-face ((t (:inherit region))))
 '(symbol-overlay-default-face ((t (:inherit (region bold))))))
