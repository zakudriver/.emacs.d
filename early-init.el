;;; Code:

;; (setenv "LIBRARY_PATH" "/opt/homebrew/lib/gcc/12:/opt/homebrew/lib/gcc/12/gcc/aarch64-apple-darwin22/12")

(setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/10:/usr/local/opt/gcc/lib/gcc/10/gcc/x86_64-apple-darwin20/10.2.0")

;; (setenv "LIBRARY_PATH" "/opt/homebrew/opt/gcc/lib/gcc/12:/opt/homebrew/opt/libgccjit/lib/gcc/12:/opt/homebrew/opt/gcc/lib/gcc/12/gcc/aarch64-apple-darwin22/12")


(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5
      native-comp-deferred-compilation nil
      native-comp-jit-compilation nil
      package-enable-at-startup nil
      use-package-enable-imenu-support t
      frame-inhibit-implied-resize t ;; Inhibit resizing frame
      byte-compile-warnings '(cl-functions) ;; clear waring: Package cl is deprecated
      inhibit-startup-message t
      initial-major-mode 'fundamental-mode
      load-prefer-newer noninteractive)


;; (add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(undecorated-round . t))


;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
(setq-default mode-line-format nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
