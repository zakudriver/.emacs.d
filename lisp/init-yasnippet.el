;; init-yasnippet.el --- Initialize yasnippet configurations.	-*- lexical-binding: t -*-


;;; Commentary:
;; some configuration of yasnippet.

;;; Code:


(use-package yasnippet
  :hook
  (after-init . yas-global-mode)
  :custom
  (yas-keymap-disable-hook t)
  :bind (:map yas-minor-mode-map
              ("TAB" . nil)
              ("<tab>" . nil)))


(use-package yasnippet-snippets)


;; Yasnippet Completion At Point Function
;; (use-package yasnippet-capf
;;   :init (add-to-list 'completion-at-point-functions #'yasnippet-capf))


(provide 'init-yasnippet)

;;; init-yasnippet.el ends here
