;;; init-flymake --- Summary

;;; Commentary:
;; some configuration of flymake.

;;; Code:


(use-package flymake
  :hook
  (prog-mode . flymake-mode)
  :custom
  (flymake-fringe-indicator-position 'right-fringe)
  :config
  (add-to-list 'elisp-flymake-byte-compile-load-path load-path))


(use-package flymake-diagnostic-at-point
  :commands flymake-diagnostic-at-point-mode
  :hook
  (flymake-mode . flymake-diagnostic-at-point-mode)
  :custom
  (flymake-diagnostic-at-point-display-diagnostic-function #'flymake-diagnostic-at-point-display-posframe)
  :config
  (defvar flymake-posframe-buffer " *flymake-posframe-buffer*"
    "Name of the flymake posframe buffer.")
  (defun flymake-diagnostic-at-point-display-posframe (text)
    "Display the flymake diagnostic TEXT inside a child frame."
    (posframe-show
     flymake-posframe-buffer
     :string (propertize
              (concat flymake-diagnostic-at-point-error-prefix text)
              'face (if-let ((type (get-char-property (point) 'flymake-diagnostic)))
                        (pcase (flymake--diag-type type)
                          (:error 'error)
                          (:warning 'warning)
                          (:note 'success)
                          (_ 'default))
                      'default))
	   :left-fringe 4
	   :right-fringe 4
     :max-width (round (* (frame-width) 0.62))
     :max-height (round (* (frame-height) 0.62))
     :internal-border-width 1
     ;; :internal-border-color (face-background 'posframe-border nil t)
     :background-color (face-background 'tooltip nil t))
    (unwind-protect
        (push (read-event) unread-command-events)
      (progn
        (posframe-hide flymake-posframe-buffer)
        (other-frame 0)))))


(use-package flymake-eslint
  :config
  (defun my/flymake-eslint-enable-maybe ()
    "Enable `flymake-eslint' based on the project configuration.
Search for the project ESLint configuration to determine whether the buffer
should be checked."
    (when-let* ((root (locate-dominating-file (buffer-file-name) "package.json"))
                (rc (locate-file ".eslintrc" (list root) '(".js" ".json"))))
      (make-local-variable 'exec-path)
      (push (file-name-concat root "node_modules" ".bin") exec-path)
      (print exec-path)
      (flymake-eslint-enable))))


(provide 'init-flymake)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flymake.el ends here
