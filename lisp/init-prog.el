;;; Code:


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
