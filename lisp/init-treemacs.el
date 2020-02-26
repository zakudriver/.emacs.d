;;; Code:


;; treemacs
(use-package treemacs
  :bind
  (:map treemacs-mode-map
        ("M-o" . treemacs-visit-node-no-split)
        ("M-c" . treemacs-collapse-parent-node)
        ("M-f" . treemacs-create-file)
        ("M-d" . treemacs-create-dir)
        ("M-v" . treemacs-visit-node-vertical-split)
        ("M-h" . treemacs-visit-node-horizontal-split)
        ("M-r" . treemacs-rename)
        ("C-x C-d" . treemacs-delete))
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

  (use-package treemacs-projectile
    :after projectile
    :bind (:map projectile-command-map
                ("h" . treemacs-projectile)))

  (use-package treemacs-magit
    :after magit
    :commands treemacs-magit--schedule-update
    :hook
    ((magit-post-commit
      git-commit-post-finish
      magit-post-stage
      magit-post-unstage) . treemacs-magit--schedule-update))

  (use-package doom-themes
    :hook
    (treemacs-mode . doom-themes-treemacs-config)))


(provide 'init-treemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-treemacs.el ends here
