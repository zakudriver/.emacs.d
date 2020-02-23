;;; Code:


(use-package ibuffer
  :ensure nil
  :functions
  (all-the-icons-icon-for-file
   all-the-icons-icon-for-mode
   all-the-icons-auto-mode-match?
   all-the-icons-faicon
   my-ibuffer-find-file)
  :commands
  (ibuffer-find-file
   ibuffer-current-buffer)
  :bind
  (("C-x C-b" . ibuffer)
   :map ibuffer-mode-map
   ("j" . ibuffer-forward-line)
   ("k" . ibuffer-backward-line)
   ("h" . ibuffer-do-kill-lines)
   ("p" . ibuffer-jump-to-buffer))
  :custom
  (ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  (ibuffer-formats `((mark modified read-only (locked)
                           ;; Here you may adjust by replacing :right with :center or :left
                           ;; According to taste, if you want the icon further from the name
                           " " (icon 2 2 :left :elide)
                           ,(propertize " " 'display `(space :align-to 8))
                           (name 30 40 :left :elide)
                           " " (size 9 -1 :right)
                           " " (mode 20 20 :left :elide) " " filename-and-process)
                     (mark " " (name 16 -1) " " filename)))
  :config
  (with-eval-after-load 'counsel
    (advice-add #'ibuffer-find-file :override #'(lambda ()
                                                  (interactive)
                                                  (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                                                             (if (buffer-live-p buf)
                                                                                 (with-current-buffer buf
                                                                                   default-directory)
                                                                               default-directory))))
                                                    (counsel-find-file default-directory))
                                                  )))

  ;; Group ibuffer's list by project root
  (use-package ibuffer-projectile
    :functions all-the-icons-octicon ibuffer-do-sort-by-alphabetic
    :hook
    (ibuffer-mode . (lambda ()
                      (ibuffer-projectile-set-filter-groups)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic))))
    :custom
    (ibuffer-projectile-prefix
     (if (display-graphic-p)
         (concat
          (all-the-icons-octicon "file-directory"
                                 :face ibuffer-filter-group-name-face
                                 :v-adjust -0.05
                                 :height 1.25)
          " ")
       "Project: ")))
  )


(provide 'init-ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ibuffer.el ends here
