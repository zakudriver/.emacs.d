;;; init-package --- Summary

;;; Commentary:
;; some configuration of package.

;;; Code:

(eval-when-compile
  (require 'init-custom))


;; FIXME: DO NOT copy package-selected-packages to init/custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(with-eval-after-load 'package
  (defun package--save-selected-packages (&optional value)
    "Set and (don't!) save `package-selected-packages' to VALUE."
    (when value
      (setq package-selected-packages value))
    (unless after-init-time
      (add-hook 'after-init-hook #'package--save-selected-packages))))


;; ELPA: refer to https://elpa.emacs-china.org/
(defvar-local package-archives-list '(melpa emacs-china tuna))
(defun switch-package-archives (archives)
  "Switch to specific package ARCHIVES repository."
  (interactive
   (list
    (intern (completing-read "Switch to archives: "
                             package-archives-list))))
  (cond
   ((eq kumo/package-archives 'melpa)
    (setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                             ("melpa" . "http://melpa.org/packages/"))))
   ((eq kumo/package-archives 'emacs-china)
    (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                             ("melpa" . "http://elpa.emacs-china.org/melpa/"))))
   ((eq kumo/package-archives 'netease)
    (setq package-archives '(("gnu"   . "http://mirrors.163.com/elpa/gnu/")
                             ("melpa" . "http://mirrors.163.com/elpa/melpa/"))))
   ((eq kumo/package-archives 'melpa-mirror)
    (setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                             ("melpa" . "http://www.mirrorservice.org/sites/melpa.org/packages/"))))
   ((eq kumo/package-archives 'tuna)
    (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                             ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))))

(switch-package-archives kumo/package-archives)


;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))


;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(eval-and-compile
  (setq use-package-always-ensure t
        use-package-always-defer t
        use-package-expand-minimally t
        use-package-enable-imenu-support t))


(eval-when-compile
  (require 'use-package))


(use-package comp
  :ensure nil
  :config
  (defun kumo-package-native-compile-async (package &optional all)
    "Compile PACKAGE natively, or with prefix ALL, all packages."
    (interactive (list (unless current-prefix-arg
			                   (completing-read "Package: " (mapcar #'car package-alist)))))
    (let* ((directory (if package
			                    (file-name-directory (locate-library package))
			                  package-user-dir)))
      (native-compile-async directory kumo/native-compile-async-jobs t))))


;; Required by `use-package'
(use-package diminish)
(use-package bind-key)


;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)


;; Auto update packages
(use-package auto-package-update
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-interval 7)
  (auto-package-update-hide-results t)
  :init
  (auto-package-update-maybe))


(provide 'init-package)

;;; init-package.el ends here
