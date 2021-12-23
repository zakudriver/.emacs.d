;;; init-lisp --- Summary

;;; Commentary:
;; some configuration of Lisp languages.

;;; Code:


(use-package paredit
  :hook
  ((emacs-lisp-mode clojure-mode) . paredit-mode))


;; Show function arglist or variable docstring
(use-package eldoc
  :ensure nil
  :config
  (with-no-warnings
    ;; Display `eldoc' in child frame
    (when (and (require 'posframe nil t) (posframe-workable-p))
      (defvar eldoc-posframe-buffer "*eldoc-posframe-buffer*"
        "The posframe buffer name use by eldoc-posframe.")

      (defvar eldoc-posframe-hide-posframe-hooks
        '(pre-command-hook post-command-hook focus-out-hook)
        "The hooks which should trigger automatic removal of the posframe.")

      (defvar eldoc-posframe-delay 0.2
        "Delay seconds to display `eldoc'.")

      (defvar-local eldoc-posframe--timer nil)

      (defun eldoc-posframe-hide-posframe ()
        "Hide messages currently being shown if any."
        (when eldoc-posframe--timer
          (cancel-timer eldoc-posframe--timer))

        (posframe-hide eldoc-posframe-buffer)
        (dolist (hook eldoc-posframe-hide-posframe-hooks)
          (remove-hook hook #'eldoc-posframe-hide-posframe t)))

      (defun eldoc-posframe-show-posframe (str &rest args)
        "Display STR with ARGS."
        (when eldoc-posframe--timer
          (cancel-timer eldoc-posframe--timer))

        (posframe-hide eldoc-posframe-buffer)
        (dolist (hook eldoc-posframe-hide-posframe-hooks)
          (add-hook hook #'eldoc-posframe-hide-posframe nil t))

        (setq eldoc-posframe--timer
              (run-with-idle-timer
               eldoc-posframe-delay nil
               (lambda ()
                 (when str
                   (posframe-show
                    eldoc-posframe-buffer
                    :string (concat (propertize "\n" 'face '(:height 0.3))
                                    (apply #'format str args)
                                    (propertize "\n\n" 'face '(:height 0.3)))
                    :postion (point)
                    :left-fringe 8
                    :right-fringe 8
                    :width (round (* (frame-width) 0.62))
                    :height (round (* (frame-height) 0.62))
                    :poshandler #'posframe-poshandler-point-bottom-left-corner-upward
                    :internal-border-width 1
                    :internal-border-color (face-foreground 'font-lock-comment-face nil t)
                    :background-color (face-background 'tooltip nil t)))))))
      (add-hook 'emacs-lisp-mode-hook
                (lambda ()
                  (setq-local eldoc-message-function #'eldoc-posframe-show-posframe))))))


;;;; emacs-lisp-mode
(use-package elisp-mode
  :ensure nil
  :bind
  (:map emacs-lisp-mode-map
        ("C-<tab>" . kumo-indent-whole-buffer)))


(use-package highlight-quoted
  :hook
  (emacs-lisp-mode . highlight-quoted-mode))


(use-package package-lint)


;; Syntax highlighting of known Elisp symbols
(use-package highlight-defined
  :hook
  (emacs-lisp-mode . highlight-defined-mode)
  :custom
  (highlight-defined-face-use-itself t))


;;;; clojure-mode
(use-package clojure-mode)


(use-package cider
  :hook
  (cider-mode . eldoc-mode)
  :custom
  (cider-repl-history-file "~/.emacs.d/cider-history")
  (cider-repl-history-size 3000)
  (cider-repl-result-prefix ";; => ")
  (cider-show-error-buffer nil))


(use-package clj-refactor
  :hook
  (clojure-mode . clj-refactor-mode))


(provide 'init-lisp)

;;; init-lisp.el ends here
