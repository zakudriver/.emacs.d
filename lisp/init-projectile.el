;;; init-projectile --- Summary

;;; Commentary:
;; some configuration of projectile.

;;; Code:


(eval-when-compile
  (require 'init-const))


;; Manage and navigate projects
(use-package projectile
  :bind
  ("C-c P" . projectile-command-map)
  :hook
  (after-init . projectile-mode)
  :custom
  (projectile-mode-line-prefix "")
  (projectile-sort-order 'recentf)
  (projectile-use-git-grep t)
  :config
  ;; (projectile-update-mode-line)         ; Update mode-line at the first time

  ;; Use the faster searcher to handle project files: ripgrep `rg'.
  (setq projectile-generic-command
        (let ((rg-cmd ""))
          (dolist (dir projectile-globally-ignored-directories)
            (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
          (concat "rg -0 --files --color=never --hidden" rg-cmd)))

  ;; Faster searching on Windows
  (when sys/win32p
    (when (or (executable-find "fd") (executable-find "rg"))
      (setq projectile-indexing-method 'alien
            projectile-enable-caching nil))
    (setq projectile-git-submodule-command nil))

  ;; Support Perforce project
  (let ((val (or (getenv "P4CONFIG") ".p4config")))
    (add-to-list 'projectile-project-root-files-bottom-up val)))


(use-package find-file-in-project
  :bind
  ("C-c p f" . find-file-in-project-by-selected)
  ("C-c p F" . find-file-with-similar-name)
  ("C-c p d" . find-directory-in-project-by-selected)
  :custom
  (ffip-use-rust-fd t))


(provide 'init-projectile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-projectile.el ends here
