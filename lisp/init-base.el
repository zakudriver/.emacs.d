;;; Code:


(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(defvar current-directory default-directory
  "emacs startup directory")

;; Prefers the newest version of a file
(setq load-prefer-newer t)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

(setq mouse-drag-copy-region t)


;; Environment
(progn
  (setenv "PATH" (concat (getenv "PATH") ":" (mapconcat 'identity kumo/env-path ":")))
  (dolist (i kumo/env-path)
    (add-to-list 'exec-path i)))

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))


(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
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
  :hook (after-init . savehist-mode)
  :custom
  (enable-recursive-minibuffers t)
  (history-length 1000)
  (savehist-autosave-interval 300)
  (savehist-additional-variables '(mark-ring
                                   global-mark-ring
                                   search-ring
                                   regexp-search-ring
                                   extended-command-history)))


;; open init.el
(defun open-init-file()
  "Open init.el file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f2>") 'open-init-file)



(provide 'init-base)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-basic.el ends here
