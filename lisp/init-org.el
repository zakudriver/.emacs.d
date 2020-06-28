;;; Code:


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
  (org-todo-keywords
   '((sequence "TODO(t)" "DOING(i)" "HANGUP(h)" "|" "DONE(d)" "CANCEL(c)")
     (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)")))
  (org-todo-keyword-faces '(("HANGUP" . warning)
                            ("❓" . warning)))
  (org-priority-faces '((?A . error)
                        (?B . warning)
                        (?C . success)))
  :config
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

  (use-package easy-hugo)

  (use-package ox-hugo
    :after ox)
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


(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
