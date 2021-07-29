;;; Code:


;; linum-mode
(global-linum-mode t)


;; prettify-symbols-mod
;; (global-prettify-symbols-mode t)


;; Miscs
(setq uniquify-buffer-name-style 'post-forward-angle-brackets  ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      delete-by-moving-to-trash t    ; Deleting files go to OS's trash folder
      make-backup-files nil          ; Forbide to make backup files
      set-mark-command-repeat-pop t  ; Repeating C-SPC after popping mark pops it again
      auto-save-default nil
      completion-ignore-case t)


;; Permanently indent with spaces, never with TABs
(setq-default tab-width 2
              c-basic-offset 2
              indent-tabs-mode nil
              major-mode 'text-mode)


;; Automatically reload files was modified by external program
(use-package autorevert
  :hook
  (after-init . global-auto-revert-mode))


;; Treat undo history as a tree
(use-package undo-tree
  :hook
  (after-init . global-undo-tree-mode)
  (web-mode . undo-tree-mode) ;; web-mode
  )


;; Show number of matches in mode-line while searching
(use-package anzu
  :diminish
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
   :map isearch-mode-map
   ([remap isearch-query-replace] . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook
  (after-init . global-anzu-mode))


;; Increase selected region by semantic units
(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))


;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :bind
  ([remap comment-dwim] . comment-dwim-2))


;; Multiple cursors
(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ("C-M->" . mc/skip-to-next-like-this)
   ("C-M-<" . mc/skip-to-previous-like-this)
   :map mc/keymap
   ("C-|" . mc/vertical-align-with-space)))


;; Automatic parenthesis pairing
(use-package elec-pair
  :hook
  (after-init . electric-pair-mode)
  :custom
  (electric-pair-pairs
   '((?\" . ?\")
     (?\' . ?\')
     (?\` . ?\`)
     (?\{ . ?\})))
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))


;; Smartly select region, rectangle, multi cursors
(use-package smart-region
  :hook
  (after-init . smart-region-on))


;; Hungry deletion
(use-package hungry-delete
  :hook
  (after-init . global-hungry-delete-mode)
  :bind
  (:map hungry-delete-mode-map
        ("M-<backspace>" . hungry-delete-backward))
  :custom
  (hungry-delete-except-modes '(help-mode minibuffer-inactive-mode calc-mode ivy-mode minibuffer-mode))
  (hungry-delete-chars-to-skip " \t\f\v"))


;; move to the beginning/end of line or code
(use-package mwim
  :bind
  ("C-a" . 'mwim-beginning-of-code-or-line)
  ("C-e" . 'mwim-end-of-code-or-line)
  ("C-c m w" . 'avy-goto-char-timer)
  ("C-c m l" . 'avy-goto-char-in-line)
  ("C-c m n" . 'avy-goto-line-below)
  ("C-c m p" . 'avy-goto-line-above))


;; Flexible text folding
(use-package origami
  :hook
  (prog-mode . origami-mode)
  :bind
  (:map origami-mode-map
        ("C-. o" . origami-open-node)
        ("C-. c" . origami-close-node))
  :custom
  (origami-show-fold-header t)
  :config
  (face-spec-reset-face 'origami-fold-header-face))


(use-package whole-line-or-region
  :hook
  (after-init . whole-line-or-region-global-mode))


(use-package goto-chg
  :bind
  ("M-o" . 'goto-last-change)
  ("M-O" . 'goto-last-change-reverse))


(use-package hideshow
  :hook
  (prog-mode . hs-minor-mode)
  :bind
  (:map hs-minor-mode-map
        ("C-c @ a" . 'hs-show-all)
        ("C-c @ h" . 'hs-hide-all)
        ("C-c @ d" . 'hs-hide-block)
        ("C-c @ s" . 'hs-show-block)
        ("C-c @ t" . 'hs-toggle-hiding)))


(use-package selectric-mode
  :hook
  (after-init . selectric-mode))


(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
