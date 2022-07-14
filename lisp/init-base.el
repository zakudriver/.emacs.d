;;; init-base --- Summary

;;; Commentary:
;; some basal configuration.

;;; Code:


(eval-when-compile
  (require 'init-const)
  (require 'init-custom))


;; utf-8 group
(setq locale-coding-system     'utf-8)
(prefer-coding-system          'utf-8)
(set-language-environment      'utf-8)
(set-default-coding-systems    'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system   'utf-8)
(set-file-name-coding-system   'utf-8)
(set-keyboard-coding-system    'utf-8)
(set-terminal-coding-system    'utf-8)
(set-selection-coding-system   'utf-8)
(modify-coding-system-alist    'process "*" 'utf-8)


(setq load-prefer-newer                        t          ;; Prefers the newest version of a file
      large-file-warning-threshold             100000000  ;; Prefers the newest version of a file
      ring-bell-function                       'ignore    ;; disable the annoying bell ring
      mouse-drag-copy-region                   t
      create-lockfiles                         nil
      read-process-output-max                  (* 1024 1024 2)
      native-comp-async-report-warnings-errors nil)


;; Environment
(when kumo/env-path
  (setenv "PATH" (concat (getenv "PATH") ":" (mapconcat 'identity kumo/env-path ":")))
  (dolist (i kumo/env-path)
    (add-to-list 'exec-path i)))


;; History
(use-package saveplace
  :hook
  (after-init . save-place-mode))


(use-package recentf
  :hook
  (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 25)
  (recentf-exclude '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                     "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                     "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                     "^/tmp/" "^/var/folders/.+$" ; "^/ssh:"
                     (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude))


(use-package savehist
  :hook
  (after-init . savehist-mode)
  :custom
  (enable-recursive-minibuffers  t)
  (history-length                1000)
  (savehist-autosave-interval    300)
  (savehist-additional-variables '(mark-ring
                                   global-mark-ring
                                   search-ring
                                   regexp-search-ring
                                   extended-command-history)))


;; which-key
(use-package which-key
  :hook
  (after-init . which-key-mode)
  :custom
  (which-key-popup-type 'minibuffer)
  (which-key-sort-order 'which-key-prefix-then-key-order))


;; so-long emacs/>=27p
(use-package so-long
  :hook
  (after-init . global-so-long-mode)
  :custom
  (so-long-threshold 400))


(provide 'init-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-base.el ends here
