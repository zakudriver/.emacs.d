 ;;;Code:


(use-package dart-mode
  :defines
  (projectile-project-root-files-bottom-up)
  :config
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
    (add-to-list 'projectile-project-root-files-bottom-up "BUILD")))


;; (use-package hover
;;   :after dart-mode
;;   :bind
;;   (:map dart-mode-map
;;         ("C-M-z" . #'hover-run-or-hot-reload)
;;         ("C-M-x" . #'hover-run-or-hot-restart)
;;         ("C-M-p" . #'hover-take-screenshot'))
;;   :init
;;   (setq flutter-sdk-path (concat (getenv "HOME") "/flutter")
;;         hover-command-path (concat (getenv "GOPATH") "/bin/hover")
;;         hover-hot-reload-on-save t
;;         hover-screenshot-path (concat (getenv "HOME") "/Pictures"
;;                                       hover-screenshot-prefix "my-prefix-"
;;                                       hover-observatory-uri "http://my-custom-host:50300"))
;;   )


(provide 'init-dart)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dart.el ends here
