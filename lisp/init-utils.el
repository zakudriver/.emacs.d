;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(use-package elpa-mirror
  :custom
  (elpamr-default-output-directory "~/.emacs.d/myelpa/"))

(use-package magit
  :bind
  (:map magit-status-mode-map
        ("q" . (lambda ()
                 (interactive)
                 (kill-buffer)
                 (jump-to-register :magit-fullscreen))))
  :config
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows)))


(use-package docker)


(use-package vterm
  :bind
  (:map vterm-mode-map
        ("M-p" . vterm-send-prior)
        ("M-n" . vterm-send-next)
        ("M-k" . vterm-send-up)
        ("M-p" . vterm-send-down))
  :config
  (use-package vterm-toggle
    :custom
    (vterm-toggle-fullscreen-p nil)
    :init
    (add-to-list 'display-buffer-alist
                 '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                   (display-buffer-reuse-window display-buffer-in-side-window)
                   (side . bottom)
                   ;;(dedicated . t) ;dedicated is supported in emacs27
                   (reusable-frames . visible)
                   (window-height . 0.3)))))


(provide 'init-utils)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
