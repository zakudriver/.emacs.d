;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(use-package elpa-mirror
  :custom
  (elpamr-default-output-directory "~/.emacs.d/myelpa/"))

(use-package magit
  :bind
  (:map magit-status-mode-map
        ("q" . (lambda ()
                 (interactive)
                 (kill-buffer)
                 (jump-to-register :magit-fullscreen))))
  :config
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows)))


(use-package docker
  :commands docker)


(use-package vterm
  :bind
  (:map vterm-mode-map
        ("M-p" . vterm-send-prior)
        ("M-n" . vterm-send-next)
        ("M-k" . vterm-send-up)
        ("M-p" . vterm-send-down))
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

(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
