;; init-dired.el --- Initialize dired configurations.	-*- lexical-binding: t -*-


;;; Commentary:
;; some configuration of Dired.

;;; Code:


(eval-when-compile
  (require 'init-const))


;; Directory operations
(use-package dired
  :ensure nil
  :bind
  (:map dired-mode-map
        ("k" . dired-previous-line)
        ("j" . dired-next-line))
  :custom
  (dired-dwim-target       t)
  (dired-recursive-deletes 'always)
  (dired-recursive-copies  'always)
  :config
  (when sys/macp
    ;; Suppress the warning: `ls does not support --dired'.
    (setq dired-use-ls-dired nil)

    (when (executable-find "gls")
      ;; Use GNU ls as `gls' from `coreutils' if available.
      (setq insert-directory-program "gls")))

  (when (or (and sys/macp (executable-find "gls"))
            (and sys/linuxp (executable-find "ls")))
    ;; Using `insert-directory-program'
    (setq ls-lisp-use-insert-directory-program t)
    ;; Show directory first
    (setq dired-listing-switches "-alh --group-directories-first")

    ;; Quick sort dired buffers via hydra
    (use-package dired-quick-sort
      :bind
      (:map dired-mode-map
            ("C-. s" . hydra-dired-quick-sort/body))))

  ;; Show git info in dired
  (use-package dired-git-info
    :bind
    (:map dired-mode-map
          ("C-. g" . dired-git-info-mode)))

  ;; Allow rsync from dired buffers
  (use-package dired-rsync
    :bind
    (:map dired-mode-map
          ("C-. r" . dired-rsync)))

  ;; Colourful dired
  (use-package diredfl
    :hook
    (dired-mode . diredfl-global-mode))


  ;; Shows icons
  (use-package nerd-icons-dired
    :custom-face
    (nerd-icons-dired-dir-face ((t (:inherit nerd-icons-dsilver :foreground unspecified))))
    :hook
    (dired-mode . nerd-icons-dired-mode))


  ;; Extra Dired functionality
  (use-package dired-aux
    :ensure nil)

  (use-package dired-x
    :ensure nil
    :demand
    :config
    (let ((cmd (cond
                (sys/mac-x-p "open")
                (sys/linux-x-p "xdg-open")
                (t ""))))
      (setq dired-guess-shell-alist-user
            `(("\\.pdf\\'" ,cmd)
              ("\\.docx\\'" ,cmd)
              ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
              ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
              ("\\.\\(?:xcf\\)\\'" ,cmd)
              ("\\.csv\\'" ,cmd)
              ("\\.tex\\'" ,cmd)
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
              ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
              ("\\.html?\\'" ,cmd)
              ("\\.md\\'" ,cmd))))

    (setq dired-omit-files
          (concat dired-omit-files
                  "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))))


(use-package fd-dired)


(provide 'init-dired)

;;; init-dired.el ends here
