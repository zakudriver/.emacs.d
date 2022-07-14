;;; Code:

(setenv "LIBRARY_PATH" "/opt/homebrew/lib/gcc/11:/opt/homebrew/lib/gcc/11/gcc/aarch64-apple-darwin20/11.1.0")

;; (setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/10:/usr/local/opt/gcc/lib/gcc/10/gcc/x86_64-apple-darwin20/10.2.0")


(setq
 gc-cons-threshold most-positive-fixnum
 gc-cons-percentage 0.5
 package-enable-at-startup nil
 frame-inhibit-implied-resize t ;; Inhibit resizing frame
 byte-compile-warnings '(cl-functions) ;; clear waring: Package cl is deprecated
 inhibit-startup-message t
 initial-major-mode 'fundamental-mode
 load-prefer-newer noninteractive)


(advice-add #'package--ensure-init-file :override #'ignore)


(add-to-list 'default-frame-alist '(undecorated . t))


;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
