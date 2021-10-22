;;; init-ruby --- Summary

;;; Commentary:
;; some configuration of ruby.

;;; Code:


(setq ruby-indent-level 2
      ruby-indent-tabs-mode nil)


;; Integrate rbenv
(use-package rbenv
  :hook
  (after-init . global-rbenv-mode)
  :custom
  (rbenv-show-active-ruby-in-modeline nil)
  (rbenv-executable "rbenv"))


;; Run a Ruby process in a buffer
(use-package inf-ruby
  :hook
  ((ruby-mode . inf-ruby-minor-mode)
   (compilation-filter . inf-ruby-auto-enter)))


;; Ruby YARD comments
(use-package yard-mode
  :diminish
  :hook
  (ruby-mode . yard-mode))


(provide 'init-ruby)

;;; init-ruby.el ends here
