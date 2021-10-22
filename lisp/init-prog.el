;;; init-prog --- Summary

;;; Commentary:
;; some configuration of prog.

;;; Code:


;; (use-package prog-mode
;;   :hook
;;   (prog-mode . prettify-symbols-mode)
;;   :custom
;;   (prettify-symbols-unprettify-at-point 'right-edge)
;;   :init
;;   (setq-default prettify-symbols-alist kumo/prettify-symbols-alist))


(use-package vimrc-mode)


(use-package dockerfile-mode)


(use-package bazel-mode
  :ensure nil
  :mode
  (("\\.bzl$" . bazel-mode)
   ("\\.bazel" . bazel-mode)
   ("^WORKSPACE$" . bazel-mode)))


(use-package protobuf-mode
  :mode
  (("\\.proto$" . protobuf-mode)))


(use-package yaml-mode)


(use-package graphql-mode)


(provide 'init-prog)

;;; init-prog.el ends here
