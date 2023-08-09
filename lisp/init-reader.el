;;; init-window --- Summary


;;; Commentary:
;;
;; EPUB readers.
;;

;;; Code:


(use-package nov
  :mode
  ("\\.epub\\'" . nov-mode)
  :hook
  (nov-mode . my-nov-setup)
  :bind
  (:map nov-mode-map
        ("j" . next-line)
        ("k" . previous-line))
  :config
  (defun my-nov-setup ()
    "Setup `nov-mode' for better reading experience."
    (visual-line-mode 1)
    ;; (olivetti-mode 1)
    (face-remap-add-relative 'variable-pitch :family "Times New Roman" :height 1.5))
  
  (with-no-warnings
    ;; WORKAROUND: errors while opening `nov' files with Unicode characters
    ;; @see https://github.com/wasamasa/nov.el/issues/63
    (defun my-nov-content-unique-identifier (content)
      "Return the the unique identifier for CONTENT."
      (let* ((name (nov-content-unique-identifier-name content))
             (selector (format "package>metadata>identifier[id='%s']"
                               (regexp-quote name)))
             (id (car (esxml-node-children (esxml-query selector content)))))
        (and id (intern id))))
    (advice-add #'nov-content-unique-identifier :override #'my-nov-content-unique-identifier)))


;; (use-package mixed-pitch
;;   :hook
;;   ((text-mode nov-mode) . mixed-pitch-mode))


(use-package olivetti
  :custom
  (olivetti-body-width 0.9))


(provide 'init-reader)

;;; init-reader.el ends here
