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


(use-package docker)


(use-package vterm
  :bind
  (:map vterm-mode-map
        ("M-p" . vterm-send-prior)
        ("M-n" . vterm-send-next)
        ("M-k" . vterm-send-up)
        ("M-p" . vterm-send-down))
  :config
  (defun kumo-check-vterm-window-live ()
    (catch 'break
      (dolist (i (window-list))
        (let ((name (buffer-name (window-buffer i))))
          (when (string-match-p "vterm" name)
            ;; (delete-window name)
            (throw 'break i)
            ))))
    )

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
                                      ))
              )
  )


(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
