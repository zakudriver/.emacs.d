;;; Code:


(use-package evil
  :init
  (evil-mode t)
  :custom
  (evil-want-C-u-scroll t)
  (x-select-enable-clipboard t)
  :config
  ;; normal no yank
  (define-key evil-normal-state-map (kbd "d") 'evil-delete-no-yank)
  ;; (define-key evil-normal-state-map (kbd "p") 'evil-paste-after-no-yank)
  ;; visual yank
  (define-key evil-visual-state-map (kbd "d") 'evil-delete)
  ;; redefine evil operator
  (evil-define-key 'insert global-map (kbd "DEL") 'hungry-delete-backward)
  (evil-define-key 'visual global-map (kbd "z") 'kumo-wrap-with-input)

  ;; redefine evil operator function
  (evil-define-operator evil-delete-char-no-yank (beg end type register yank-handler)
    "Delete next character without yanking."
    :motion evil-forward-char
    (evil-delete beg end type ?_ yank-handler))

  (evil-define-operator evil-delete-backward-char-no-yank (beg end type register yank-handler)
    "Delete backward character without yanking."
    :motion evil-backward-char
    (evil-delete beg end type ?_ yank-handler))

  (evil-define-operator evil-change-no-yank (beg end type register yank-handler delete-func)
      "Change text from BEG to END with TYPE.
    Save in REGISTER or the kill-ring with YANK-HANDLER.
    DELETE-FUNC is a function for deleting text, default `evil-delete'.
    If TYPE is `line', insertion starts on an empty line.
    If TYPE is `block', the inserted text in inserted at each line
    of the block.(without yanking)"
    (let ((delete-func (or delete-func #'evil-delete))
          (nlines (1+ (evil-count-lines beg end)))
          (opoint (save-excursion
                    (goto-char beg)
                    (line-beginning-position))))
      (unless (eq evil-want-fine-undo t)
        (evil-start-undo-step))
      ;; (funcall delete-func beg end type register yank-handler)
      (funcall delete-func beg end type ?_ yank-handler)
      (cond
        ((eq type 'line)
          (if ( = opoint (point))
            (evil-open-above 1)
            (evil-open-below 1)))
        ((eq type 'block)
          (evil-insert 1 nlines))
        (t
          (evil-insert 1)))))

  (evil-define-operator evil-change-line-no-yank (beg end type register yank-handler)
    "Change to end of line without yanking."
    :motion evil-end-of-line-or-visual-line
    (evil-change beg end type ?_ yank-handler #'evil-delete-line))

  (evil-define-operator evil-substitute-no-yank (beg end type register yank-handler)
    "Substitute without yanking."
    :motion evil-forward-char
    (evil-change beg end type ?_ yank-handler))

  (evil-define-operator evil-substitute-whole-line-no-yank (beg end type register yank-handler)
    "Change whole line without yanking."
    :motion evil-line-or-visual-line
    ;; (evil-change beg end type register yank-handler #'evil-delete-whole-line)
    (evil-change beg end type ?_ yank-handler #'evil-delete-whole-line)
    )

  (evil-define-operator evil-delete-no-yank (beg end type register yank-handler)
    "Delete text from BEG to END with TYPE.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
    (unless register
      (let ((text (filter-buffer-substring beg end)))
        (unless (string-match-p "\n" text)
          ;; set the small delete register
          (evil-set-register ?- text))))
    ;; (let ((evil-was-yanked-without-register nil))
    ;;   ;; (evil-yank beg end type ?_ yank-handler))
    ;;   (evil-yank beg end type register yank-handler))
    (cond
     ((eq type 'block)
      (evil-apply-on-block #'delete-region beg end nil))
     ((and (eq type 'line)
         (= end (point-max))
         (or (= beg end)
             (/= (char-before end) ?\n))
         (/= beg (point-min))
         (=  (char-before beg) ?\n))
      (delete-region (1- beg) end))
     (t
      (delete-region beg end)))
    ;; place cursor on beginning of line
    (when (and (called-interactively-p 'any)
             (eq type 'line))
    (evil-first-non-blank)))

  (defun evil-paste-after-no-yank ()
    (interactive)
    (let ((evil-this-register ?0))
      (call-interactively 'evil-paste-after)))
)


(use-package general
  :config
  (define-key evil-normal-state-map (kbd "SPC") (general-simulate-key "C-c")))


(general-define-key
 :states '(normal visual)
 :prefix ","
 "," 'counsel-M-x
 "k" 'symbol-overlay-put
 "K" 'symbol-overlay-remove-all
 "u" 'undo-tree-visualize
 "o" 'overwrite-mode
 "m" 'counsel-imenu
 "w" 'avy-goto-char-timer
)
         

(general-define-key
 :prefix "C-c"
 "q" 'save-buffers-kill-terminal
 "Q" 'kill-emacs
 "j" 'avy-goto-line-below
 "k" 'avy-goto-line-above
 "R" 'kumo-rename-current-buffer-file
 "D" 'kumo-delete-current-buffer-file
 "ps" 'treemacs-select-window
 "pt" 'treemacs
 "pp" 'projectile-command-map
 "sr" 'counsel-rg
 "ss" 'swiper
 "ff" 'counsel-find-file
 "fz" 'counsel-fzf
 "bb" 'counsel-switch-buffer
 "bi" 'ibuffer
 "bt" 'kumo-kill-this-buffer
 "bo" 'kumo-kill-other-buffers
 "ba" 'kumo-kill-all-buffers
 "bp" 'kumo-switch-to-previous-buffer
 "bs" 'save-buffer
 "bn" 'kumo-new-temp-buffer
 "c" 'flycheck-list-errors
 "d" 'dired
 "D" 'docker
 "ww" 'hydra-frame-window/body
 "wv" 'split-window-below
 "wh" 'split-window-right
 "wd" 'delete-window
 "mo" 'multi-term
 "md" 'kumo-multi-term-dedicated-open
 "mc" 'multi-term-dedicated-close
 "mp" 'multi-term-dedicated-prev
 "mn" 'multi-term-dedicated-next
 "mt" 'multi-term-dedicated-toggle
 "ms" 'multi-term-dedicated-select
 "gs" 'magit-status
 ;; "gb"  'magit-blame-echo
 ;; "gm"  'magit-dispatch-popup
)


(general-define-key
 :states '(normal visual)
 "j" 'evil-next-visual-line
 "k" 'evil-previous-visual-line
 "H" 'mwim-beginning-of-code-or-line
 "L" 'mwim-end-of-code-or-line
 "f" 'avy-goto-char-in-line
 "gb" 'pop-tag-mark
 "c" 'evil-change-no-yank
 "C" 'evil-change-line-no-yank
 "x" 'evil-delete-char-no-yank
 "X" 'evil-delete-backward-char-no-yank
 "s" 'evil-substitute-no-yank
 "S" 'evil-substitute-whole-line-no-yank
)


(provide 'init-evil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-evil.el ends here
