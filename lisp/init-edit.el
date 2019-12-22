(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Miscs
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
(setq make-backup-files nil)               ; Forbide to make backup files
(setq auto-save-default nil)               ; Disable auto save
(setq set-mark-command-repeat-pop t)       ; Repeating C-SPC after popping mark pops it again

(setq-default major-mode 'text-mode)

(add-hook 'abbrev-mode-hook (lambda () (diminish 'abbrev-mode)))

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :hook (after-init . global-auto-revert-mode))

;; Jump to Chinese characters
(use-package ace-pinyin
  :diminish ace-pinyin-mode
  :hook (after-init . ace-pinyin-global-mode))

;; An all-in-one comment command to rule them all
(use-package comment-dwim-2)

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode))

;; Hungry deletion
(use-package hungry-delete
  :diminish hungry-delete-mode
  :hook (after-init-hook . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; Move to the beginning/end of line or code
(use-package mwim)

;; Treat undo history as a tree
(use-package undo-tree
  :diminish undo-tree-mode
  :hook (after-init . global-undo-tree-mode))

;; treemacs
(use-package treemacs
  :diminish treemacs-mode
  :hook (after-init . treemacs-tag-follow-mode))

(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
