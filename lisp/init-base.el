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


;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))


(setq system-time-locale              "C"
      delete-by-moving-to-trash       t    ; Deleting files go to OS's trash folder
      inhibit-compacting-font-caches  t
      make-backup-files               nil  ; Forbide to make backup files
      auto-save-default               nil
      uniquify-buffer-name-style      'post-forward-angle-brackets  ; Show path if names are same
      sentence-end                    "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space       nil
      word-wrap-by-category           t

      ring-bell-function              'ignore  ;; disable the annoying bell ring
      mouse-drag-copy-region          t
      create-lockfiles                nil
      ;; Increase how much is read from processes in a single chunk (default is 4kb)
      read-process-output-max         #x10000  ;; 64kb
      
      adaptive-fill-regexp            "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$")


;; Garbage Collector Magic Hack
(use-package gcmh
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold #x1000000)) ; 16MB


;; Environment
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
			  "[ \t\n]*$" "" (shell-command-to-string
					  "$SHELL --login -c 'echo $PATH'"
						    ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)


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
