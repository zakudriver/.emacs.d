
;;; Code:


;; treemacs
(use-package treemacs
  :diminish treemacs-mode
  :bind (
    :map treemacs-mode-map
    ("C-x o" . treemacs-collapse-parent-node)
    ("C-x f" . treemacs-create-file)
    ("C-x d" . treemacs-create-dir)
    ("C-x C-d" . treemacs-delete)
    ("C-x v" . treemacs-visit-node-vertical-split)
    ("C-x h" . treemacs-visit-node-horizontal-split)
    ("R" . treemacs-refresh)
  )
  :config (treemacs-resize-icons 16))


(provide 'init-treemacs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-treemacs.el ends here
