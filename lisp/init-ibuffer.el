;; init-ibuffer.el --- Initialize ibuffer configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; some configuration of ibuffer.

;;; Code:


(eval-when-compile
  (require 'init-const)
  (require 'init-custom))


(use-package ibuffer
  :commands
  (ibuffer-find-file
   ibuffer-current-buffer)
  :bind
  ("C-x C-b" . ibuffer)
  :hook
  (ibuffer . (lambda ()
               ;; (persp-ibuffer-set-filter-groups)
               (unless (eq ibuffer-sorting-mode 'alphabetic)
                 (ibuffer-do-sort-by-alphabetic))))
  :custom
  (ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  :config
  (with-eval-after-load 'counsel
    (advice-add #'ibuffer-find-file :override (lambda ()
                                                (interactive)
                                                (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                                                           (if (buffer-live-p buf)
                                                                               (with-current-buffer buf
                                                                                 default-directory)
                                                                             default-directory))))
                                                  (counsel-find-file default-directory)))))

  (use-package all-the-icons-ibuffer
    :hook
    (ibuffer-mode . all-the-icons-ibuffer-mode))

  ;; Group ibuffer's list by project root
  (use-package ibuffer-projectile
    :functions ibuffer-do-sort-by-alphabetic
    :hook
    (ibuffer . (lambda ()
                 (ibuffer-projectile-set-filter-groups)
                 (unless (eq ibuffer-sorting-mode 'alphabetic)
                   (ibuffer-do-sort-by-alphabetic))))
    :custom
    (ibuffer-projectile-prefix
     (concat
      (all-the-icons-octicon "file-directory"
                             :face ibuffer-filter-group-name-face
                             :v-adjust -0.05
                             :height 1.25)
      " "))))


(provide 'init-ibuffer)

;;; init-ibuffer.el ends here
