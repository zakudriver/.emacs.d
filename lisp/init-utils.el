;;; Code:


(eval-when-compile
  (require 'init-const)
  (require 'init-custom))


(use-package magit
  :config
  (advice-add #'magit-mode-bury-buffer :override #'(lambda ()
                                                     "Quit and kill magit buffer, then restore window configuration."
                                                     (interactive)
                                                     (let ((current (current-buffer)))
                                                       (dolist (buf (magit-mode-get-buffers))
                                                         (unless (eq buf current)
                                                           (kill-buffer buf))))
                                                     (funcall magit-bury-buffer-function t))))


(use-package docker
  :commands docker)


(use-package vterm
  :bind
  (:map vterm-mode-map
        ("M-p" . vterm-send-prior)
        ("M-n" . vterm-send-next)
        ("M-k" . vterm-send-up)
        ("M-j" . vterm-send-down)
        ("M-p" . vterm-yank)
        ("M-u" . vterm-undo))
  :config
  (advice-add #'vterm :override #'(lambda ()
                                    "Toggle vterm or create a new vterm."
                                    (interactive)

                                    (let ((buffer  (catch 'break
                                                     (dolist (i (buffer-list))
                                                       (when (string-match-p "vterm" (buffer-name i))
                                                         (throw 'break i)
                                                         )))))

                                      (if buffer
                                          (let ((win (get-buffer-window buffer)))
                                            (if (window-live-p win)
                                                (delete-window win)
                                              (let ((w (catch 'break
                                                         (dolist (i (window-list))
                                                           (let ((name (buffer-name (window-buffer i))))
                                                             (when (string-match-p "vterm" name)
                                                               ;; (delete-window name)
                                                               (throw 'break i)
                                                               ))))))
                                                (if w
                                                    (delete-window w)
                                                  (kumo-bottom-window buffer)))))
                                        (setq buffer (generate-new-buffer "vterm"))
                                        (with-current-buffer (buffer-name buffer)
                                          (vterm-mode))
                                        (kumo-bottom-window buffer)
                                        )
                                      )))

  (defvar kumo/vterm-buffer-list nil
    "Vterm buffer list.")

  (defun vterm-exit-hook()
    "Vterm exit hook."
    (when (derived-mode-p 'vterm-mode)
      (setq kumo/vterm-buffer-list
	          (delq (current-buffer) kumo/vterm-buffer-list))))
  (add-hook 'kill-buffer-hook #'vterm-exit-hook)

  (defun vterm-mode-hook()
    "Hook for `vterm-mode-hook'."
    (add-to-list 'kumo/vterm-buffer-list (current-buffer)))
  (add-hook 'vterm-mode-hook #'vterm-mode-hook)

  (defun kumo-vterm-switch (direction offset)
    (if kumo/vterm-buffer-list
        (let ((len (length kumo/vterm-buffer-list))
	            (index (cl-position (current-buffer) kumo/vterm-buffer-list)))
	        (if index
	            (let ((target-index (if (eq direction 'previous)
				                              (mod (+ index offset) len)
				                            (mod (- index offset) len))))
	              (switch-to-buffer (nth target-index kumo/vterm-buffer-list) nil t))
	          (switch-to-buffer (car kumo/vterm-buffer-list) nil t)))
      nil))

  (defun kumo-vterm-previous (&optional offset)
    "Go to the previous term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET."
    (interactive "P")
    (kumo-vterm-switch 'previous (or offset 1)))

  (defun kumo-vterm-next (&optional offset)
    "Go to the next term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET."
    (interactive "P")
    (kumo-vterm-switch 'next (or offset 1)))
  )


;; process view
(use-package proced
  :bind
  (:map proced-mode-map
        ("k" . previous-line)
        ("j" . next-line)
        ("G" . proced-toggle-auto-update))
  :custom
  (proced-auto-update-flag t)
  (proced-auto-update-interval 3)
  :init
  (setq-default proced-format 'verbose))


;; Youdao Dictionary
(use-package youdao-dictionary
  :bind
  (:map youdao-dictionary-mode-map
        ("?" . youdao-dictionary-hydra/body))
  :custom
  (url-automatic-caching t)
  (youdao-dictionary-use-chinese-word-segmentation t) ; 中文分词
  )


;; Fast search tool `ripgrep'
(use-package rg
  :defines projectile-command-map
  :hook
  (rg-mode . rg-enable-default-bindings)
  :bind
  (("C-c R" . rg-menu)
   :map rg-global-map
   ("c" . rg-dwim-current-dir)
   ("f" . rg-dwim-current-file)
   :map rg-mode-map
   ("m" . rg-menu))
  :custom
  (rg-keymap-prefix "\C-cR")
  (rg-group-result t)
  (rg-show-columns t)
  :config
  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)

  (with-eval-after-load 'projectile
    (defalias 'projectile-ripgrep #'rg-project)
    (bind-key "s R" #'rg-project projectile-command-map))

  (with-eval-after-load 'counsel
    (bind-keys
     :map rg-global-map
     ("R" . counsel-rg)
     ("F" . counsel-fzf))))


(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
