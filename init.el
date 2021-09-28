;;; Code:

;; (setq debug-on-error t)


(when (version< emacs-version "26.1")
  (error "This requires Emacs 26.1 and above!"))


;; Speed up startup
(setq auto-mode-case-fold nil)

(defvar old-file-name-handler-alist file-name-handler-alist)
(unless (or (daemonp) noninteractive)
  ;; (let ((old-file-name-handler-alist file-name-handler-alist))
  ;; If `file-name-handler-alist' is nil, no 256 colors in TUI
  ;; @see https://emacs-china.org/t/spacemacs-centaur-emacs/3802/839
  (setq file-name-handler-alist
        (unless (display-graphic-p)
          '(("\\(?:\\.tzst\\|\\.zst\\|\\.dz\\|\\.txz\\|\\.xz\\|\\.lzma\\|\\.lz\\|\\.g?z\\|\\.\\(?:tgz\\|svgz\\|sifz\\)\\|\\.tbz2?\\|\\.bz2\\|\\.Z\\)\\(?:~\\|\\.~[-[:alnum:]:#@^._]+\\(?:~[[:digit:]]+\\)?~\\)?\\'" . jka-compr-handler))))
  (add-hook 'emacs-startup-hook
            (lambda ()
              "Recover file name handlers."
              (setq file-name-handler-alist
                    (delete-dups (append file-name-handler-alist
                                         old-file-name-handler-alist))))))


(defvar normal-gc-cons-threshold (* 20 1024 1024))
(let ((init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


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
  ;; ;; (require 'init-modeline)
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

  ;; ;; polymode
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
  (require 'init-web))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-parentheses-background-colors '("#F4CCE1") nil nil "Customized with use-package highlight-parentheses")
 '(highlight-parentheses-colors '("#73317a") nil nil "Customized with use-package highlight-parentheses")
 '(warning-suppress-types '((comp))))
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
(put 'upcase-region 'disabled nil)
