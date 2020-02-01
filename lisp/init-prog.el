;;; Code:

(use-package vimrc-mode)

(use-package dockerfile-mode)

(use-package bazel-mode
  :mode
  (("\\.bzl$" . bazel-mode)
   ("\\.bazel" . bazel-mode)
   ("^WORKSPACE$" . bazel-mode)))

(use-package protobuf-mode
  :mode
  (("\\.proto$" . protobuf-mode)))


(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
