;;; Code:


(eval-when-compile
  (require 'init-custom))


(use-package org
  :bind
  (:map org-mode-map
        ("C-," . nil)
        ("C-, l" . font-lock-mode)
        ("C-, c" . org-cycle-agenda-files))
  :custom
  (org-startup-indented t)
  (org-startup-with-inline-images t)
  (org-log-done t)
  (org-edit-timestamp-down-means-later t)
  (org-hide-emphasis-markers t)
  (org-catch-invisible-edits 'show)
  (org-fast-tag-selection-single-key 'expert)
  (org-tags-column 80)
  (org-image-actual-width nil)
  (org-src-fontify-natively t)
  (org-list-allow-alphabetical t)
  (truncate-lines nil)
  (org-todo-keywords
   '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
     (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)")))
  (org-todo-keyword-faces '(("HANGUP" . warning)
                            ("❓" . warning)))
  (org-priority-faces '((?A . error)
                        (?B . warning)
                        (?C . success)))
  :config
  ;; Embed inline CSS read from a file.
  (add-hook 'org-export-before-processing-hook 'kumo-org-inline-css-hook)

  (with-eval-after-load 'counsel
    (bind-key [remap org-set-tags-command] #'counsel-org-tag org-mode-map))

  (use-package org-bullets
    :hook
    (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("🌞" "🌤" "⛈" "🌈")))

  (use-package org-pomodoro
    :custom-face
    (org-pomodoro-mode-line ((t (:inherit warning))))
    (org-pomodoro-mode-line-overtime ((t (:inherit error))))
    (org-pomodoro-mode-line-break ((t (:inherit success))))
    (org-pomodoro-keep-killed-pomodoro-time t)
    :bind
    (:map org-agenda-mode-map
          ("C-. p" . org-pomodoro)))

  (use-package org-download
    :hook
    (dired-mode . org-download-enable)
    :custom
    (org-download-image-dir "./img")
    (org-download-backend "curl \"%s\" -o \"%s\""))
  )


(use-package ob-go)


(use-package ob-typescript)


(defvar org-load-language-list '((emacs-lisp . t)
                                 (shell . t)
                                 (css . t)
                                 (sass . t)
                                 (go . t)
                                 (typescript . t)))

(org-babel-do-load-languages 'org-babel-load-languages
                             org-load-language-list)


;; hugo
(use-package easy-hugo
  :commands easy-hugo
  :bind
  (:map easy-hugo-mode-map
        ("SPC" . general-simulate-C-c)
        ("G" . kumo-easy-hugo-github-deploy))
  :custom
  (easy-hugo-org-header t)
  (easy-hugo-basedir kumo/easy-hugo-basedir)
  (easy-hugo-postdir kumo/easy-hugo-postdir)
  (easy-hugo-url kumo/easy-hugo-url)
  (easy-hugo-preview-url kumo/easy-hugo-preview-url)
  (easy-hugo-github-deploy-script kumo/easy-hugo-github-deploy-script)
  (easy-hugo-default-ext ".org")
  :hook
  (easy-hugo-mode . (lambda ()
                      (evil-set-initial-state 'easy-hugo-mode 'emacs)))
  )

(use-package ox-hugo
  :after ox)


(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
