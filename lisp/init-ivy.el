;;; Code:


(use-package counsel
  :diminish ivy-mode counsel-mode
  :bind
  (("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-s" . swiper-isearch)
   ("C-S-s" . swiper-all)
   ("C-r" . swiper-isearch-backward)
   :map counsel-mode-map
   ([remap swiper] . counsel-grep-or-swiper)
   ([remap swiper-backward] . counsel-grep-or-swiper-backward)
   ([remap dired] . counsel-dired)
   ([remap set-variable] . counsel-set-variable)
   ([remap insert-char] . counsel-unicode-char)
   ([remap recentf-open-files] . counsel-recentf)

   ("C-c c z" . counsel-fzf)
   ("C-c c p" . counsel-pt)
   ("C-c r" . counsel-rg)
   
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
    :hook
    (after-init . prescient-persist-mode)
    :custom
    (prescient-filter-method '(literal regexp initialism fuzzy)))

  (use-package ivy-prescient
    :commands ivy-prescient-re-builder
    :hook
    (ivy-mode . ivy-prescient-mode)
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
        (ivy-prescient-re-builder str)))))


;; ivy icons
(use-package all-the-icons-ivy-rich
  :hook
  (ivy-mode . all-the-icons-ivy-rich-mode))


(use-package ivy-rich
  :hook
  (counsel-projectile-mode . ivy-rich-mode)
  (ivy-rich-mode . (lambda ()
                     "Use abbreviate in `ivy-rich-mode'."
                     (setq ivy-virtual-abbreviate
                           (or (and ivy-rich-mode 'abbreviate) 'name))))
  :custom
  (ivy-rich-parse-remote-buffer nil))


(provide 'init-ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ivy.el ends here
