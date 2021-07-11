;;; Code:


;; global keys
(global-set-key (kbd "<f2>") 'kumo-open-init-file)
(global-set-key (kbd "<C-tab>") 'kumo-indent-all)
(global-set-key (kbd "M-u") 'universal-argument)
(global-set-key (kbd "C-<return>") 'smart-region)
(global-set-key (kbd "C-x s") 'kumo-save-some-buffers) ;; replace 'save-some-buffer
(global-set-key (kbd "C-c w w") 'hydra-frame-window/body)
(global-set-key (kbd "C-c w v") 'split-window-vertically)
(global-set-key (kbd "C-c w V") 'kumo-window-vertically-selected)
(global-set-key (kbd "C-c w h") 'split-window-horizontally)
(global-set-key (kbd "C-c w H") 'kumo-window-horizontally-selected)
(global-set-key (kbd "C-c w t") 'delete-window)
(global-set-key (kbd "C-c w q") 'quit-window)
(global-set-key (kbd "C-c w o") 'delete-other-windows)
(global-set-key (kbd "C-c w b") 'kumo-current-buffer-bottom-window)
(global-set-key (kbd "C-c w m") 'kumo-select-minibuffer-window)


;;(global-set-key (kbd "C-<return>") 'smart-region)

;; general keys
;; (use-package generala
;;   :config
;;   (general-define-key
;;    :prefix "C-c"


;;    "u" 'undo-tree-visualize
;;    "o" 'overwrite-mode
;;    "m" 'counsel-imenu


;;    "nd" 'kumo-number-division
;;    )

;;   (general-define-key
;;    :prefix "C-c"
;;    "P" 'projectile-command-map
;;    "pf" 'find-file-in-project-by-selected
;;    "pF" 'find-file-with-similar-name
;;    "pd" 'find-directory-in-project-by-selected
;;    "pp" 'proced
;;    "d" 'dired
;;    "D" 'docker
;;    "h" 'kumo-open-dashboard
;;    "H" 'easy-hugo

;;    "yy" 'youdao-dictionary-search-at-point-posframe
;;    "yY" 'youdao-dictionary-search-at-point
;;    "yi" 'youdao-dictionary-search-from-input

;;    "ff" 'counsel-find-file

;;    "fn" 'kumo-put-file-name-on-clipboard
;;    "fd" 'kumo-open-current-file-in-finder
;;    "bb" 'counsel-switch-buffer
;;    "bi" 'ibuffer
;;    "bt" 'kumo-kill-this-buffer
;;    "bo" 'kumo-kill-other-buffers
;;    "ba" 'kumo-kill-all-buffers
;;    "bp" 'kumo-switch-to-previous-buffer
;;    "bR" 'kumo-rename-current-buffer-file
;;    "bD" 'kumo-delete-current-buffer-file
;;    "oo" 'org-switchb
;;    "oa" 'org-agenda
;;    "cc" 'kumo-flycheck-list-errors-toggle
;;    "cs" 'kumo-flycheck-list-errors-select-window
;;    "vo" 'vterm-other-window
;;    "vv" 'vterm
;;    "vN" 'kumo-new-vterm
;;    "vs" 'kumo-vterm-select-window
;;    "vp" 'kumo-vterm-previous
;;    "vn" 'kumo-vterm-next

;;    "gd" 'magit-dispatch
;;    "gb" 'magit-log-buffer-file
;;    )



(provide 'init-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-keys.el ends here

(global-set-key (kbd "C-<return>") 'smart-region)
