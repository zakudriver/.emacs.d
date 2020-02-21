;;; Code:


(eval-when-compile
  (require 'init-const)
  (require 'init-custom))


(setq load-prefer-newer t                     ;; Prefers the newest version of a file
      large-file-warning-threshold 100000000  ;; Prefers the newest version of a file
      ring-bell-function 'ignore              ;; disable the annoying bell ring
      mouse-drag-copy-region t)


;; Environment
(when kumo/env-path
  (setenv "PATH" (concat (getenv "PATH") ":" (mapconcat 'identity kumo/env-path ":")))
  (dolist (i kumo/env-path)
    (add-to-list 'exec-path i)))


;; History
(use-package saveplace
  :ensure nil
  :hook
  (after-init . save-place-mode))


(use-package recentf
  :ensure nil
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
  :ensure nil
  :hook
  (after-init . savehist-mode)
  :custom
  (enable-recursive-minibuffers t)
  (history-length 1000)
  (savehist-autosave-interval 300)
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


(defun kumo-open-init-file()
  "Open init.el file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f2>") 'kumo-open-init-file)


(provide 'init-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-basic.el ends here
