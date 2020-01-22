
;;; Code:


;; treemacs
(use-package treemacs
  :diminish treemacs-mode
  :bind (
    :map treemacs-mode-map
    ("co" . treemacs-collapse-parent-node)
    ("C-x f" . treemacs-create-file)
    ("C-x d" . treemacs-create-dir)
    ("C-x C-d" . treemacs-delete)
    ("ov" . treemacs-visit-node-vertical-split)
    ("oh" . treemacs-visit-node-horizontal-split)
    ("R" . treemacs-refresh))
  :custom
  ;; (treemacs-collapse-dirs `(if treemacs-python-executable 3 0))
  (treemacs-sorting 'alphabetic-case-insensitive-desc)
  (treemacs-follow-after-init t)
  (treemacs-is-never-other-window t)
  (treemacs-silent-filewatch t)
  (treemacs-silent-refresh t)
  (treemacs-width 30)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-resize-icons 14)

  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))


  (use-package treemacs-magit
    :after magit
    :commands treemacs-magit--schedule-update
    :hook
    ((magit-post-commit
      git-commit-post-finish
      magit-post-stage
      magit-post-unstage) . treemacs-magit--schedule-update))
)


(provide 'init-treemacs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-treemacs.el ends here
