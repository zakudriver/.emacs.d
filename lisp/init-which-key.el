;;; Code:

;; which-key
(use-package which-key
  :init 
  (which-key-mode)
  :config
  (setq which-key-popup-type 'minibuffer
        which-key-sort-order 'which-key-prefix-then-key-order)
)



(provide 'init-which-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-which-key.el ends here
