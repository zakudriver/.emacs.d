;;; Code:


(use-package polymode
  :mode
  ("\.ts$" . poly-typescript-styled-mode)
  :config
  ;; css in ts (styled-components)
  (define-hostmode poly-typescript-hostmode :mode 'typescript-mode)

  (define-innermode poly-typescript-styled-innermode
    :mode 'css-mode
    :head-matcher "\\(styled\\|css\\)[.()<>[:alnum:]]?+`"
    :tail-matcher "`;"
    :head-mode 'host
    :tail-mode 'host)

  (define-polymode poly-typescript-styled-mode
    :hostmode 'poly-typescript-hostmode
    :innermodes '(poly-typescript-styled-innermode)))


(provide 'init-polymode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-polymode.el ends here
