;;; Code:


;; Integrate rbenv
(use-package rbenv
  :hook
  (after-init . global-rbenv-mode)
  :custom
  (rbenv-show-active-ruby-in-modeline nil)
  (rbenv-executable "rbenv"))


;; Ruby YARD comments
(use-package yard-mode
  :diminish
  :hook
  (ruby-mode . yard-mode))


(provide 'init-ruby)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ruby.el ends here
