;;; init-edit --- Summary

;;; Commentary:
;; some configuration of edit.

;;; Code:


;; line number
;; (global-linum-mode t)
(global-display-line-numbers-mode t)


;; prettify-symbols-mod
;; (global-prettify-symbols-mode t)


;; Miscs
(setq set-mark-command-repeat-pop     t  ; Repeating C-SPC after popping mark pops it again
      completion-ignore-case          t
      scroll-preserve-screen-position t
      scroll-conservatively           0)


;; Permanently indent with spaces, never with TABs
(setq-default tab-width        2
              indent-tabs-mode nil
              major-mode       'text-mode)


;; Automatically reload files was modified by external program
(use-package autorevert
  :hook
  (after-init . global-auto-revert-mode))


;; Treat undo history as a tree
;; (use-package undo-tree
;;   :hook
;;   (after-init . global-undo-tree-mode)
;;   (web-mode . undo-tree-mode) ;; web-mode
;;   :init
;;   (add-to-list 'display-buffer-alist
;;                '("^ \\*undo-tree\\*"
;;                  display-buffer-reuse-window display-buffer-in-side-window
;;                  (reusable-frames . visible)
;;                  (side . right)
;;                  (slot . 1)
;;                  (window-width . 0.5)
;;                  (window-height . 0.15))))


;; Show number of matches in mode-line while searching
(use-package anzu
  :bind
  (([remap query-replace]        . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
   :map isearch-mode-map
   ([remap isearch-query-replace]        . anzu-isearch-query-replace)
   ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook
  (after-init . global-anzu-mode))


;; Increase selected region by semantic units
(use-package expand-region
  :custom
  (expand-region-fast-keys-enabled nil)
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))


;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :bind
  ([remap comment-dwim] . comment-dwim-2))


;; Multiple cursors
(use-package multiple-cursors
  :hook
  (after-init . multiple-cursors-mode)
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-c C-<"     . mc/mark-all-like-this)
   ("C-M->"       . mc/skip-to-next-like-this)
   ("C-M-<"       . mc/skip-to-previous-like-this)
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
  (hungry-delete-except-modes  '(help-mode minibuffer-inactive-mode calc-mode ivy-mode minibuffer-mode))
  (hungry-delete-chars-to-skip " \t\f\v"))


;; move to the beginning/end of line or code
(use-package mwim
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))


;; moving the cursor quickly
(use-package avy
  :bind
  ("C-S-w" . avy-goto-char-timer)
  ("C-S-l" . avy-goto-char-in-line)
  ("C-S-n" . avy-goto-line-below)
  ("C-S-p" . avy-goto-line-above))


(use-package goto-chg
  :bind
  ("M-o" . 'goto-last-change)
  ("M-O" . 'goto-last-change-reverse))


;; flexible text folding
(use-package hideshow
  :hook
  (prog-mode . hs-minor-mode)
  :bind
  (:map hs-minor-mode-map
        ("C-. @ S" . hs-show-all)
        ("C-. @ H" . hs-hide-all)
        ("C-. @ h" . hs-hide-block)
        ("C-. @ s" . hs-show-block)
        ("C-. @ t" . hs-toggle-hiding)))


(use-package whole-line-or-region
  :hook
  (after-init . whole-line-or-region-global-mode))


;; (use-package selectric-mode
;;   :hook
;;   (after-init . selectric-mode))


;; (use-package edit-server
;;   :hook
;;   (after-init . edit-server-start)
;;   (kill-emacs . edit-server-stop))


(provide 'init-edit)

;;; init-edit.el ends here
