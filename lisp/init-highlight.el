;;; Code:


(eval-when-compile
  (require 'init-const))


;; Highlight the current line
(global-hl-line-mode t)


;; Highlight matching parens
(use-package paren
  :hook
  (after-init . show-paren-mode)
  :config
  (defun display-line-overlay (pos str &optional face)
    "Display line at POS as STR with FACE.
FACE defaults to inheriting from default and highlight."
    (let ((ol (save-excursion
                (goto-char pos)
                (make-overlay (line-beginning-position)
                              (line-end-position)))))
      (overlay-put ol 'display str)
      (overlay-put ol 'face
                   (or face '(:inherit highlight)))
      ol))

  (defvar-local show-paren--off-screen-overlay nil)
  (advice-add #'show-paren-function :after #'(lambda (&rest _args)
                                               "Display matching line for off-screen paren."
                                               (when (overlayp show-paren--off-screen-overlay)
                                                 (delete-overlay show-paren--off-screen-overlay))
                                               ;; check if it's appropriate to show match info,
                                               (when (and (overlay-buffer show-paren--overlay)
                                                          (not (or cursor-in-echo-area
                                                                   executing-kbd-macro
                                                                   noninteractive
                                                                   (minibufferp)
                                                                   this-command))
                                                          (and (not (bobp))
                                                               (memq (char-syntax (char-before)) '(?\) ?\$)))
                                                          (= 1 (logand 1 (- (point)
                                                                            (save-excursion
                                                                              (forward-char -1)
                                                                              (skip-syntax-backward "/\\")
                                                                              (point))))))
                                                 ;; rebind `minibuffer-message' called by
                                                 ;; `blink-matching-open' to handle the overlay display
                                                 (cl-letf (((symbol-function #'minibuffer-message)
                                                            (lambda (msg &rest args)
                                                              (let ((msg (apply #'format-message msg args)))
                                                                (setq show-paren--off-screen-overlay
                                                                      (display-line-overlay
                                                                       (window-start) msg ))))))
                                                   (blink-matching-open)))))
  )


;; Highlight symbols
(use-package symbol-overlay
  :functions
  (turn-off-symbol-overlay turn-on-symbol-overlay)
  :custom-face
  (symbol-overlay-default-face ((t (:inherit (region bold)))))
  :bind
  (("C-c l m" . symbol-overlay-put)
   ("C-c l M" . symbol-overlay-remove-all)
   :map symbol-overlay-map
   ("M-n" . symbol-overlay-jump-next)
   ("M-p" . symbol-overlay-jump-prev)
   ("M-N" . symbol-overlay-switch-forward)
   ("M-P" . symbol-overlay-switch-backward)
   ("M-. s" . symbol-overlay-save-symbol)
   ("h" . nil)
   ("w" . nil))
  :hook
  (prog-mode . symbol-overlay-mode)
  (iedit-mode . turn-off-symbol-overlay)
  (iedit-mode-end . turn-on-symbol-overlay)
  :custom
  (symbol-overlay-idle-time 0.1)
  :init
  (with-eval-after-load 'all-the-icons
    (setq symbol-overlay-faces
          '((:inherit (all-the-icons-blue bold) :inverse-video t)
            (:inherit (all-the-icons-pink bold) :inverse-video t)
            (:inherit (all-the-icons-yellow bold) :inverse-video t)
            (:inherit (all-the-icons-maroon bold) :inverse-video t)
            (:inherit (all-the-icons-red bold) :inverse-video t)
            (:inherit (all-the-icons-orange bold) :inverse-video t)
            (:inherit (all-the-icons-green bold) :inverse-video t)
            (:inherit (all-the-icons-cyan bold) :inverse-video t))))
  :config
  ;; Disable symbol highlighting while selecting
  (advice-add #'set-mark :after #'(lambda (&rest _)
                                    "Turn off symbol highlighting."
                                    (interactive)
                                    (symbol-overlay-mode -1)))

  (advice-add #'deactivate-mark :after #'(lambda ()
                                           "Turn on symbol highlighting."
                                           (interactive)
                                           (when (derived-mode-p 'prog-mode)
                                             (symbol-overlay-mode 1))))
  )


;; Highlight indentions
(use-package highlight-indentation
  :hook
  (prog-mode . highlight-indentation-mode)
  :custom
  (highlight-indentation-blank-lines t))


;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))


(use-package highlight-parentheses
  :hook
  (prog-mode . highlight-parentheses-mode)
  :custom
  (hl-paren-colors '("#73317a"))
  (hl-paren-background-colors '("#F4CCE1")))


;; Highlight uncommitted changes using VC
(use-package diff-hl
  :hook
  (after-init . global-diff-hl-mode)
  (dired-mode . diff-hl-dired-mode)
  :custom
  (diff-hl-draw-borders nil)
  :bind
  (:map diff-hl-command-map
        ("C-. c" . diff-hl-mark-hunk)))


(use-package pulse
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region))))
  (pulse-highlight-face ((t (:inherit region)))))


(provide 'init-highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-highlight.el ends here
