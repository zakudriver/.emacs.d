;;; Code:


;; global keys
(global-set-key (kbd "<f2>") 'kumo-open-init-file)
(global-set-key (kbd "<C-tab>") 'kumo-indent-all)
(global-set-key (kbd "M-u") 'universal-argument)


;; evil keys
(use-package evil
  :init
  (evil-mode t)
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  (x-select-enable-clipboard t)
  (evil-kill-on-visual-paste nil)
  (evil-want-keybinding nil)
  :config
  (evil-set-undo-system 'undo-tree)
  ;; evil-record-macro key q -> Q
  (evil-global-set-key 'normal
                       (kbd "q") nil)
  (evil-global-set-key 'normal
                       (kbd "Q") 'evil-record-macro)

  ;; redefine evil operator
  (evil-define-key nil evil-normal-state-map
    "d" 'evil-delete-no-yank
    (kbd "C-.") nil)

  (evil-define-key nil evil-insert-state-map
    (kbd "DEL") 'hungry-delete-backward
    (kbd "<C-backspace>") 'backward-delete-char-untabify)
  
  (evil-define-key nil evil-visual-state-map
    "z" 'kumo-wrap-with-input
    "d" 'evil-delete)

  ;; evil x key
  (evil-define-operator evil-delete-char-no-yank (beg end type register yank-handler)
    "Delete next character without yanking."
    :motion evil-forward-char
    (evil-delete beg end type ?_ yank-handler))
  (advice-add #'evil-delete-char :override #'evil-delete-char-no-yank)

  ;; evil X key
  (evil-define-operator evil-delete-backward-char-no-yank (beg end type register yank-handler)
    "Delete backward character without yanking."
    :motion evil-backward-char
    (evil-delete beg end type ?_ yank-handler))
  (advice-add #'evil-delete-backward-char :override #'evil-delete-backward-char-no-yank)

  ;; evil c key
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
  (advice-add #'evil-change :override #'evil-change-no-yank)

  ;; evil C key
  (evil-define-operator evil-change-line-no-yank (beg end type register yank-handler)
    "Change to end of line without yanking."
    :motion evil-end-of-line-or-visual-line
    (evil-change beg end type ?_ yank-handler #'evil-delete-line))
  (advice-add #'evil-change-line :override #'evil-change-line-no-yank)

  ;; evil s key
  (evil-define-operator evil-substitute-no-yank (beg end type register yank-handler)
    "Substitute without yanking."
    :motion evil-forward-char
    (evil-change beg end type ?_ yank-handler))
  (advice-add #'evil-substitute :override #'evil-substitute-no-yank)

  ;; evil S key
  (evil-define-operator evil-substitute-whole-line-no-yank (beg end type register yank-handler)
    "Change whole line without yanking."
    :motion evil-line-or-visual-line
    ;; (evil-change beg end type register yank-handler #'evil-delete-whole-line)
    (evil-change beg end type ?_ yank-handler #'evil-delete-whole-line))
  (advice-add #'evil-substitute-whole-line :override #'evil-substitute-whole-line-no-yank)

  ;; evil d key
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
  )


(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :init
  (evil-collection-init))


;; general keys 
(use-package general
  :config
  (define-key evil-normal-state-map (kbd "SPC") (general-simulate-key "C-c"))

  (general-define-key
   :states '(normal visual)
   :prefix ","
   "k" 'symbol-overlay-put
   "K" 'symbol-overlay-remove-all
   "u" 'undo-tree-visualize
   "o" 'overwrite-mode
   "m" 'counsel-imenu
   "w" 'avy-goto-char-timer
   "s" 'kumo-save-some-buffers
   "nd" 'kumo-number-division
   )
  
  (general-define-key
   :prefix "C-c"
   "p" 'projectile-command-map
   "P" 'proced
   "d" 'dired
   "D" 'docker
   "h" 'kumo-open-dashboard
   "H" 'easy-hugo
   "R" 'rg-menu
   "yy" 'youdao-dictionary-search-at-point-posframe
   "yY" 'youdao-dictionary-search-at-point
   "yi" 'youdao-dictionary-search-from-input
   "sr" 'counsel-rg
   "ss" 'swiper
   "ff" 'counsel-find-file
   "fr" 'counsel-recentf
   "fz" 'counsel-fzf
   "bb" 'counsel-switch-buffer
   "bi" 'ibuffer
   "bt" 'kumo-kill-this-buffer
   "bo" 'kumo-kill-other-buffers
   "ba" 'kumo-kill-all-buffers
   "bp" 'kumo-switch-to-previous-buffer
   "bR" 'kumo-rename-current-buffer-file
   "bD" 'kumo-delete-current-buffer-file
   "oo" 'org-switchb
   "oa" 'org-agenda
   "cc" 'kumo-flycheck-list-errors-toggle
   "cs" 'kumo-flycheck-list-errors-select-window
   "ww" 'hydra-frame-window/body
   "wv" 'split-window-vertically
   "wV" 'kumo-window-vertically-selected
   "wh" 'split-window-horizontally
   "wH" 'kumo-window-horizontally-selected
   "wt" 'delete-window
   "wq" 'quit-window
   "wo" 'delete-other-windows
   "wb" 'kumo-current-buffer-bottom-window
   "wm" 'kumo-select-minibuffer-window
   "wn" 'evil-window-new
   "vo" 'vterm-other-window
   "vv" 'vterm
   "vN" 'kumo-new-vterm
   "vs" 'kumo-vterm-select-window
   "vp" 'kumo-vterm-previous
   "vn" 'kumo-vterm-next
   "gs" 'magit-status
   "gd" 'magit-dispatch
   "gb" 'magit-log-buffer-file
   )

  (general-define-key
   :states '(normal visual)
   "H" 'mwim-beginning-of-code-or-line
   "L" 'mwim-end-of-code-or-line
   "J" 'avy-goto-line-below
   "K" 'avy-goto-line-above
   "f" 'avy-goto-char-in-line
   "gb" 'pop-tag-mark
   ))


(provide 'init-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-keys.el ends here
