;;; Code:

;; Golang
;;
;; Go packages:
;; go get -u github.com/nsf/gocode
;; go get -u github.com/rogpeppe/godef
;; go get -u github.com/golang/lint/golint
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u golang.org/x/tools/cmd/guru
;; go get -u golang.org/x/tools/cmd/gorename
;; go get -u golang.org/x/tools/cmd/godoc
;; go get -u github.com/derekparker/delve/cmd/dlv
;; go get -u github.com/josharian/impl
;; go get -u github.com/cweill/gotests/...
;; go get -u github.com/fatih/gomodifytags
;; go get -u github.com/davidrjenni/reftools/cmd/fillstruct
;;
(use-package go-mode
  :bind (:map go-mode-map
              ([remap xref-find-definitions] . godef-jump))

  :config
  (use-package go-guru
    :bind (:map go-mode-map
                ([remap xref-find-references] . go-guru-referrers)))

  (use-package company-go
    :init (add-to-list 'company-backends (company-backend-with-yas 'company-go))
    :after company))

(general-define-key
 :states '(normal visual)
 :keymaps 'go-mode-map
 "gd" 'godef-jump
 "gr" 'go-guru-referrers)

(provide 'init-go)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-go.el ends here
