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
      auto-save-default nil)


;; Permanently indent with spaces, never with TABs
(setq-default tab-width 2
              c-basic-offset 2
              indent-tabs-mode nil
              major-mode 'text-mode)


;; prettify-symbols
;; (use-package prog-mode
;;   :ensure nil
;;   :hook
;;   (prog-mode . prettify-symbols-mode)
;;   :custom
;;   (prettify-symbols-unprettify-at-point 'right-edge)
;;   :init
;;   (setq-default prettify-symbols-alist kumo/prettify-symbols-alist))


;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :hook
  (after-init . global-auto-revert-mode))


;; Treat undo history as a tree
(use-package undo-tree
  :diminish undo-tree-mode
  :hook
  (after-init . global-undo-tree-mode)
  (web-mode . undo-tree-mode)
  :bind
  (:map undo-tree-map
        ("C-/" . nil)))


(use-package expand-region
  :bind ("C-=" . er/expand-region))


;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :bind
  ("C-/" . comment-dwim-2)
  ([remap comment-dwim] . comment-dwim-2))


(use-package elec-pair
  :ensure nil
  :hook
  (after-init . electric-pair-mode)
  :custom
  (electric-pair-pairs
   '((?\" . ?\")
     (?\' . ?\')
     (?\` . ?\`)
     (?\{ . ?\})))
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))


;; Hungry deletion
;; (use-package hungry-delete
;;   :diminish
;;   :hook
;;   (after-init . global-hungry-delete-mode)
;;   :custom
;;   (hungry-delete-chars-to-skip " \t\f\v"))


;; Move to the beginning/end of line or code
(use-package mwim)


(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
