;;; Code:


(eval-when-compile
  (require 'init-const)
  (require 'init-custom))


(use-package elpa-mirror
  :custom
  (elpamr-default-output-directory "~/.emacs.d/myelpa/"))


(use-package magit
  :config
  (advice-add #'magit-mode-bury-buffer :override #'(lambda ()
                                                     "Quit and kill magit buffer, then restore window configuration."
                                                     (interactive)
                                                     (let ((current (current-buffer)))
                                                       (dolist (buf (magit-mode-get-buffers))
                                                         (unless (eq buf current)
                                                           (kill-buffer buf))))
                                                     (funcall magit-bury-buffer-function t)))

  (use-package evil-magit
    :hook
    (magit-mode . evil-magit-init)
    :bind
    (:map magit-mode-map
          ("M-0" . nil)
          ("M-1" . nil)
          ("M-2" . nil)
          ("M-3" . nil)
          ("M-4" . nil)
          ("M-5" . nil)
          ("M-6" . nil)
          ("M-7" . nil)
          ("M-8" . nil)
          ("M-9" . nil)
          ("M-u" . nil)
          :map magit-revision-mode-map
          ("M-0" . nil)
          ("M-1" . nil)
          ("M-2" . nil)
          ("M-3" . nil)
          ("M-4" . nil)
          ("M-5" . nil)
          ("M-6" . nil)
          ("M-7" . nil)
          ("M-8" . nil)
          ("M-9" . nil)
          ("M-u" . nil)
          )))


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


(use-package proced
  :bind
  (:map proced-mode-map
        ("k" . previous-line)
        ("j" . next-line)
        ("q" . quit-and-kill-window)
        ("G" . proced-toggle-auto-update))
  :custom
  (proced-auto-update-flag t))


(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
