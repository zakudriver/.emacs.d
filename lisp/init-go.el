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
         ([remap xref-find-definitions] . godef-jump)
         ("C-c R" . go-remove-unused-imports))
  :init 
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'lsp-deferred)
  :config
  (setq gofmt-command "goimports")
  ;; Env vars
  ;; (with-eval-after-load 'exec-path-from-shell
  ;;   (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))

  (use-package company-go
    :init (add-to-list 'company-backends (company-backend-with-yas 'company-go))
    :after company)


  ;; Install or update tools
  (defvar go--tools '("golang.org/x/tools/cmd/goimports"
                      "golang.org/x/tools/cmd/gorename"

                      ;; "github.com/rogpeppe/godef"
                      "github.com/go-delve/delve/cmd/dlv"
                      "github.com/josharian/impl"
                      "github.com/cweill/gotests/..."
                      "github.com/fatih/gomodifytags"
                      "github.com/davidrjenni/reftools/cmd/fillstruct"
                      "github.com/golangci/golangci-lint/cmd/golangci-lint")
    "All necessary go tools.")

  ;; Do not use the -u flag for gopls, as it will update the dependencies to incompatible versions
  ;; https://github.com/golang/tools/blob/master/gopls/doc/user.md#installation
  (defvar go--tools-no-update '("golang.org/x/tools/gopls@latest")
    "All necessary go tools without update the dependencies.")

  (defun go-update-tools ()
    "Install or update go tools."
    (interactive)
    (unless (executable-find "go")
      (user-error "Unable to find `go' in `exec-path'!"))

    (message "Installing go tools...")
    (let ((proc-name "go-tools")
          (proc-buffer "*Go Tools*"))
      (dolist (pkg go--tools-no-update)
        (set-process-sentinel
         (start-process proc-name proc-buffer "go" "get" "-v" pkg)
         (lambda (proc _)
           (let ((status (process-exit-status proc)))
             (if (= 0 status)
                 (message "Installed %s" pkg)
               (message "Failed to install %s: %d" pkg status))))))

      (dolist (pkg go--tools)
        (set-process-sentinel
         (start-process proc-name proc-buffer "go" "get" "-u" "-v" pkg)
         (lambda (proc _)
           (let ((status (process-exit-status proc)))
             (if (= 0 status)
                 (message "Installed %s" pkg)
               (message "Failed to install %s: %d" pkg status))))))))
)


(provide 'init-go)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-go.el ends here
