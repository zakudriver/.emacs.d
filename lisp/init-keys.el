;;; init-keys --- Summary

;;; Commentary:
;; some configuration of keys.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom interactive function
(global-set-key (kbd "<f2>") 'my-open-init-file)

(global-set-key (kbd "C-c w v") 'split-window-vertically)
(global-set-key (kbd "C-c w V") 'my-window-vertically-selected)
(global-set-key (kbd "C-c w h") 'split-window-horizontally)
(global-set-key (kbd "C-c w H") 'my-window-horizontally-selected)
(global-set-key (kbd "C-c w t") 'delete-window)
(global-set-key (kbd "C-c w q") 'quit-window)
(global-set-key (kbd "C-c w o") 'delete-other-windows)
(global-set-key (kbd "C-c w b") 'my-current-buffer-bottom-window)
(global-set-key (kbd "C-c w m") 'my-select-minibuffer-window)

(global-set-key (kbd "C-c n d") 'my-number-division)

(global-set-key (kbd "C-c f n") 'my-put-file-name-on-clipboard)
(global-set-key (kbd "C-c f d") 'my-open-current-file-in-finder)

(global-set-key (kbd "C-c b t") 'my-kill-this-buffer)
(global-set-key (kbd "C-c b o") 'my-kill-other-buffers)
(global-set-key (kbd "C-c b R") 'my-rename-current-buffer-file)
(global-set-key (kbd "C-c b D") 'my-delete-current-buffer-file)
(global-set-key (kbd "C-c b P") 'my-switch-to-previous-buffer)

(global-set-key (kbd "C-c z") 'my-wrap-with-input)

;; like vim
(global-set-key (kbd "C-c d i w") 'my-delete-word-at-point)
(global-set-key (kbd "C-c y i w") 'my-save-word-at-point)
(global-set-key (kbd "C-o") 'my-newline-next-current)
(global-set-key (kbd "C-S-o") 'my-newline-above-current)

;; replace key
(global-set-key (kbd "C-<backspace>") 'my-backward-delete-word) ;; replace C-<backspace>
(global-set-key (kbd "M-d") 'my-delete-word) ;; replace M-d
(global-set-key (kbd "C-k") 'my-kill-line) ;; replace C-k

;; replace func
(global-set-key (kbd "C-x s") 'my-save-some-buffers) ;; replace 'save-some-buffer

;; extension
(global-set-key (kbd "C-S-k") 'my-kill-whole-line)
(global-set-key (kbd "C-M-m") 'my-goto-matching-bracket)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; build-in interactive
(global-set-key (kbd "C-S-a") 'beginning-of-buffer) ;; modify M-<
(global-set-key (kbd "C-S-e") 'end-of-buffer) ;; modify M->

(global-set-key (kbd "M-F") 'forward-to-word)
(global-set-key (kbd "M-B") 'backward-to-word)

(global-set-key (kbd "C-c b p") 'previous-buffer)
(global-set-key (kbd "C-c b n") 'next-buffer)

(global-set-key (kbd "C-?") 'undo-redo)

(provide 'init-keys)

;;; init-keys.el ends here
