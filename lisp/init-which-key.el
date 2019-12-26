;;; Code:

;; which-key
(use-package which-key
  :init 
  (which-key-setup-side-window-right)
  (which-key-mode)
  :config
  (setq which-key-paging-key "<f5>")
  (setq which-key-idle-secondary-delay 0.05)
  )



(provide 'init-which-key)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-which-key.el ends here