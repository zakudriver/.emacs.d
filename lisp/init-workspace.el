;;; init-workspace.el --- Initialize workspace configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Workspace configurations.
;;

;;; Code:

(use-package tabspaces
  :hook
  (after-init . tabspaces-mode)
  :custom
  (tab-bar-show nil)
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-initialize-project-with-todo nil)
  (tabspaces-session t)
  (tabspaces-session-auto-restore t)
  :init
  ;; Filter Buffers for Consult-Buffer
  (with-eval-after-load 'consult
    ;; hide full buffer list (still available with "b" prefix)
    (consult-customize consult--source-buffer :hidden t :default nil)
    ;; set consult-workspace buffer list
    (defvar consult--source-workspace
      (list :name     "Workspace Buffer"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
                                  :predicate #'tabspaces--local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name)))

      "Set workspace buffer list for consult-buffer.")
    (add-to-list 'consult-buffer-sources 'consult--source-workspace)))


(use-package find-file-in-project
  :bind
  ("C-c p f" . find-file-in-project-by-selected)
  ("C-c p F" . find-file-with-similar-name)
  ("C-c p d" . find-directory-in-project-by-selected)
  :custom
  (ffip-use-rust-fd t))


(provide 'init-workspace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-workspace.el ends here
