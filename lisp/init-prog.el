;;; Code:


(use-package vimrc-mode
  :ensure nil)


(use-package dockerfile-mode
  :ensure nil)


(use-package bazel-mode
  :ensure nil
  :mode
  (("\\.bzl$" . bazel-mode)
   ("\\.bazel" . bazel-mode)
   ("^WORKSPACE$" . bazel-mode)))


(use-package protobuf-mode
  :ensure nil
  :mode
  (("\\.proto$" . protobuf-mode)))


(use-package yaml-mode
  :ensure nil)


(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
