;;; Code:


(use-package counsel
  :diminish ivy-mode counsel-mode
  :bind
  (("C-s" . 'swiper)
   ("C-x C-r" . 'counsel-recentf)
   ("C-h f" . 'counsel-describe-function)
   ("C-h v" . 'counsel-describe-variable)
   :map ivy-minibuffer-map
   ([escape] . minibuffer-keyboard-quit)
   :map swiper-map
   ([escape] . minibuffer-keyboard-quit))
  :hook
  (after-init . ivy-mode)
  (ivy-mode . counsel-mode)
  :custom
  (enable-recursive-minibuffers t) ; Allow commands in minibuffers
  (ivy-use-selectable-prompt t)
  (ivy-use-virtual-buffers t)   ; Enable bookmarks and recentf
  (ivy-height 10)
  (ivy-fixed-height-minibuffer t)
  (ivy-count-format "(%d/%d) ")
  (ivy-on-del-error-function nil)
  (ivy-initial-inputs-alist nil)
  (swiper-action-recenter t)
  (counsel-find-file-at-point t)
  (counsel-yank-pop-separator "\n────────\n")
  (counsel-grep-base-command "rg -S --no-heading --line-number --color never %s %s")
  :config
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read))

  (when (and sys/macp (executable-find "gls"))
    (setq counsel-find-file-occur-use-find nil
          counsel-find-file-occur-cmd
          "gls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 gls -d --group-directories-first"))

  ;; Ivy integration for Projectile
  (use-package counsel-projectile
    :hook
    (counsel-mode . counsel-projectile-mode)
    :custom
    (counsel-projectile-grep-initial-input '(ivy-thing-at-point)))

  (use-package amx
    :custom
    (amx-history-length 20))

  ;; Better sorting and filtering
  (use-package prescient
    :commands prescient-persist-mode
    :custom
    (prescient-filter-method '(literal regexp initialism fuzzy))
    :init
    (prescient-persist-mode t))

  ;; Integrate yasnippet
  (use-package ivy-yasnippet
    :commands ivy-yasnippet--preview
    :bind
    ("C-c C-y" . ivy-yasnippet)
    :config
    (advice-add #'ivy-yasnippet--preview :override #'ignore))

  (use-package ivy-prescient
    :commands ivy-prescient-re-builder
    :custom-face
    (ivy-minibuffer-match-face-1 ((t (:inherit font-lock-doc-face :foreground nil))))
    :custom
    (ivy-prescient-retain-classic-highlighting t)
    (ivy-re-builders-alist
     '((counsel-ag . ivy-prescient-non-fuzzy)
       (counsel-rg . ivy-prescient-non-fuzzy)
       (counsel-pt . ivy-prescient-non-fuzzy)
       (counsel-grep . ivy-prescient-non-fuzzy)
       (counsel-imenu . ivy-prescient-non-fuzzy)
       (counsel-yank-pop . ivy-prescient-non-fuzzy)
       (swiper . ivy-prescient-non-fuzzy)
       (swiper-isearch . ivy-prescient-non-fuzzy)
       (swiper-all . ivy-prescient-non-fuzzy)
       (lsp-ivy-workspace-symbol . ivy-prescient-non-fuzzy)
       (lsp-ivy-global-workspace-symbol . ivy-prescient-non-fuzzy)
       (insert-char . ivy-prescient-non-fuzzy)
       (counsel-unicode-char . ivy-prescient-non-fuzzy)
       (t . ivy-prescient-re-builder)))
    (ivy-prescient-sort-commands
     '(:not swiper swiper-isearch ivy-switch-buffer
            counsel-grep counsel-git-grep counsel-ag counsel-imenu
            counsel-yank-pop counsel-recentf counsel-buffer-or-recentf))
    :init
    (defun ivy-prescient-non-fuzzy (str)
      "Generate an Ivy-formatted non-fuzzy regexp list for the given STR.
This is for use in `ivy-re-builders-alist'."
      (let ((prescient-filter-method '(literal regexp)))
        (ivy-prescient-re-builder str)))

    (ivy-prescient-mode t))
  )


;; More friendly display transformer for Ivy
(use-package ivy-rich
  :hook
  (counsel-projectile-mode . ivy-rich-mode)
  (ivy-rich-mode . (lambda ()
                     (setq ivy-virtual-abbreviate
                           (or (and ivy-rich-mode 'abbreviate) 'name))))
  :custom
  (ivy-rich-parse-remote-buffer nil)
  (ivy-rich-display-transformers-list
   '(ivy-switch-buffer
     (:columns
      ((ivy-rich-buffer-icon)
       (ivy-rich-candidate (:width 30))
       (ivy-rich-switch-buffer-size (:width 7))
       (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
       (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
       (ivy-rich-switch-buffer-project (:width 15 :face success))
       (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
      :predicate
      (lambda (cand) (get-buffer cand))
      :delimiter "\t")
     ivy-switch-buffer-other-window
     (:columns
      ((ivy-rich-buffer-icon)
       (ivy-rich-candidate (:width 30))
       (ivy-rich-switch-buffer-size (:width 7))
       (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
       (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
       (ivy-rich-switch-buffer-project (:width 15 :face success))
       (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
      :predicate
      (lambda (cand) (get-buffer cand))
      :delimiter "\t")
     counsel-switch-buffer
     (:columns
      ((ivy-rich-buffer-icon)
       (ivy-rich-candidate (:width 30))
       (ivy-rich-switch-buffer-size (:width 7))
       (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
       (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
       (ivy-rich-switch-buffer-project (:width 15 :face success))
       (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
      :predicate
      (lambda (cand) (get-buffer cand))
      :delimiter "\t")
     counsel-switch-buffer-other-window
     (:columns
      ((ivy-rich-buffer-icon)
       (ivy-rich-candidate (:width 30))
       (ivy-rich-switch-buffer-size (:width 7))
       (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
       (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
       (ivy-rich-switch-buffer-project (:width 15 :face success))
       (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
      :predicate
      (lambda (cand) (get-buffer cand))
      :delimiter "\t")
     persp-switch-to-buffer
     (:columns
      ((ivy-rich-buffer-icon)
       (ivy-rich-candidate (:width 30))
       (ivy-rich-switch-buffer-size (:width 7))
       (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
       (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
       (ivy-rich-switch-buffer-project (:width 15 :face success))
       (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
      :predicate
      (lambda (cand) (get-buffer cand))
      :delimiter "\t")
     counsel-M-x
     (:columns
      ((ivy-rich-function-icon)
       (counsel-M-x-transformer (:width 50))
       (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
     counsel-describe-function
     (:columns
      ((ivy-rich-function-icon)
       (counsel-describe-function-transformer (:width 50))
       (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
     counsel-describe-variable
     (:columns
      ((ivy-rich-variable-icon)
       (counsel-describe-variable-transformer (:width 50))
       (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
     counsel-set-variable
     (:columns
      ((ivy-rich-variable-icon)
       (counsel-describe-variable-transformer (:width 50))
       (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
     counsel-apropos
     (:columns
      ((ivy-rich-symbol-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-info-lookup-symbol
     (:columns
      ((ivy-rich-symbol-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-descbinds
     (:columns
      ((ivy-rich-keybinding-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-find-file
     (:columns
      ((ivy-rich-file-icon)
       (ivy-read-file-transformer))
      :delimiter "\t")
     counsel-file-jump
     (:columns
      ((ivy-rich-file-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-dired
     (:columns
      ((ivy-rich-file-icon)
       (ivy-read-file-transformer))
      :delimiter "\t")
     counsel-dired-jump
     (:columns
      ((ivy-rich-file-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-el
     (:columns
      ((ivy-rich-symbol-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-fzf
     (:columns
      ((ivy-rich-file-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-git
     (:columns
      ((ivy-rich-file-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-recentf
     (:columns
      ((ivy-rich-file-icon)
       (ivy-rich-candidate (:width 0.8))
       (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))
      :delimiter "\t")
     counsel-buffer-or-recentf
     (:columns
      ((ivy-rich-file-icon)
       (counsel-buffer-or-recentf-transformer (:width 0.8))
       (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))
      :delimiter "\t")
     counsel-bookmark
     (:columns
      ((ivy-rich-bookmark-type)
       (ivy-rich-bookmark-name (:width 40))
       (ivy-rich-bookmark-info))
      :delimiter "\t")
     counsel-bookmarked-directory
     (:columns
      ((ivy-rich-file-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-package
     (:columns
      ((ivy-rich-package-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-fonts
     (:columns
      ((ivy-rich-font-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-major
     (:columns
      ((ivy-rich-function-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-find-library
     (:columns
      ((ivy-rich-library-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-load-library
     (:columns
      ((ivy-rich-library-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-load-theme
     (:columns
      ((ivy-rich-theme-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-world-clock
     (:columns
      ((ivy-rich-world-clock-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-tramp
     (:columns
      ((ivy-rich-tramp-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-git-checkout
     (:columns
      ((ivy-rich-git-branch-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-list-processes
     (:columns
      ((ivy-rich-process-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-projectile-switch-project
     (:columns
      ((ivy-rich-file-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     counsel-projectile-find-file
     (:columns
      ((ivy-rich-file-icon)
       (counsel-projectile-find-file-transformer))
      :delimiter "\t")
     counsel-projectile-find-dir
     (:columns
      ((ivy-rich-project-icon)
       (counsel-projectile-find-dir-transformer))
      :delimiter "\t")
     counsel-minor
     (:columns
      ((ivy-rich-mode-icon)
       (ivy-rich-candidate))
      :delimiter "\t")
     treemacs-projectile
     (:columns
      ((ivy-rich-file-icon)
       (ivy-rich-candidate))
      :delimiter "\t")))
  :init
  ;; Setting tab size to 1, to insert tabs as delimiters
  ;; (add-hook 'minibuffer-setup-hook
  ;;           (lambda ()
  ;;             (setq tab-width 1)))

  (defun ivy-rich-bookmark-name (candidate)
    (car (assoc candidate bookmark-alist)))

  (defun ivy-rich-buffer-icon (candidate)
    "Display buffer icons in `ivy-rich'."
    (when (display-graphic-p)
      (let* ((buffer (get-buffer candidate))
             (buffer-file-name (buffer-file-name buffer))
             (major-mode (buffer-local-value 'major-mode buffer))
             (icon (with-current-buffer buffer (all-the-icons-icon-for-buffer))))
        (if (symbolp icon)
            (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
          icon))))

  (defun ivy-rich-file-icon (candidate)
    "Display file icons in `ivy-rich'."
    (when (display-graphic-p)
      (let* ((path (concat ivy--directory candidate))
             (file (file-name-nondirectory path))
             (icon (cond
                    ((file-directory-p path)
                     (all-the-icons-icon-for-dir path nil ""))
                    ((string-match "^/.*:$" path)
                     (all-the-icons-octicon "radio-tower" :height 1.0 :v-adjust 0.01))
                    ((not (string-empty-p file))
                     (all-the-icons-icon-for-file file :v-adjust -0.05)))))
        (if (symbolp icon)
            (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.8 :v-adjust 0.0)
          icon))))

  (defun ivy-rich-project-icon (_candidate)
    "Display project icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01)))

  (defun ivy-rich-mode-icon (_candidate)
    "Display mode icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-blue)))

  (defun ivy-rich-function-icon (_candidate)
    "Display function icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-faicon "cube" :height 0.95 :v-adjust -0.05 :face 'all-the-icons-purple)))

  (defun ivy-rich-variable-icon (_candidate)
    "Display the variable icon in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-octicon "tag" :height 0.95 :v-adjust 0 :face 'all-the-icons-lblue)))

  (defun ivy-rich-symbol-icon (_candidate)
    "Display the symbol icon in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-octicon "gear" :height 0.9 :v-adjust -0.05)))

  (defun ivy-rich-theme-icon (_candidate)
    "Display the theme icon in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-material "palette" :height 1.0 :v-adjust -0.2)))

  (defun ivy-rich-keybinding-icon (_candidate)
    "Display the keybindings icon in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-material "keyboard" :height 0.9 :v-adjust -0.15)))

  (defun ivy-rich-library-icon (_candidate)
    "Display the library icon in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-material "view_module" :height 1.0 :v-adjust -0.225 :face 'all-the-icons-lblue)))

  (defun ivy-rich-package-icon (_candidate)
    "Display the package icon in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-faicon "archive" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-silver)))

  (defun ivy-rich-font-icon (_candidate)
    "Display the font icon in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-faicon "font" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue)))

  (defun ivy-rich-world-clock-icon (_candidate)
    "Display the world clock icon in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-faicon "globe" :height 0.9 :v-adjust -0.05 :face 'all-the-icons-lblue)))

  (defun ivy-rich-tramp-icon (_candidate)
    "Display the tramp icon in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-octicon "radio-tower" :height 0.9 :v-adjust 0.01)))

  (defun ivy-rich-git-branch-icon (_candidate)
    "Display the git branch icon in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-octicon "git-branch" :height 1.0 :v-adjust -0.05 :face 'all-the-icons-green)))

  (defun ivy-rich-process-icon (_candidate)
    "Display the process icon in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-faicon "bolt" :height 1.0 :v-adjust -0.05 :face 'all-the-icons-lblue)))

  (when (display-graphic-p)
    (advice-add #'ivy-rich-bookmark-type :override #'(lambda (candidate)
                                                       (let ((filename (ivy-rich-bookmark-filename candidate)))
                                                         (cond ((null filename)
                                                                (all-the-icons-material "block" :height 1.0 :v-adjust -0.2 :face 'warning))  ; fixed #38
                                                               ((file-remote-p filename)
                                                                (all-the-icons-octicon "radio-tower" :height 0.9 :v-adjust 0.01))
                                                               ((not (file-exists-p filename))
                                                                (all-the-icons-material "block" :height 1.0 :v-adjust -0.2 :face 'error))
                                                               ((file-directory-p filename)
                                                                (all-the-icons-octicon "file-directory" :height 0.9 :v-adjust -0.05))
                                                               (t (all-the-icons-icon-for-file (file-name-nondirectory filename) :height 0.9 :v-adjust -0.05)))))))
  )


(provide 'init-ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ivy.el ends here
