;;; Code:


(use-package polymode
  :ensure nil
  :mode
  ("\\.ts$" . poly-typescript-mode)
  ("\\.tsx$" . poly-web-mode)
  ("\\.jsx$" . poly-web-mode)
  :config
  ;; == typescript ==
  (define-hostmode poly-typescript-hostmode :mode 'typescript-mode)
  (define-polymode poly-typescript-mode
    :hostmode 'poly-typescript-hostmode
    :innermodes '(poly-typescript-styled-innermode poly-typescript-graphql-innermode))

  ;; == tsx/jsx ==
  (define-hostmode poly-web-hostmode :mode 'web-mode)
  (define-polymode poly-web-mode
    :hostmode 'poly-web-hostmode
    :innermodes '(poly-typescript-styled-innermode poly-typescript-graphql-innermode))

  ;; css in ts (styled-components)
  (define-innermode poly-typescript-styled-innermode
    :mode 'css-mode
    :head-matcher "\\(styled\\|css\\)[.()<>[:alnum:]]?+`"
    :tail-matcher "`;"
    :head-mode 'host
    :tail-mode 'host)

  ;; graphql
  (define-innermode poly-typescript-graphql-innermode
    :mode 'graphql-mode
    :head-matcher "gr?a?p?h?ql`"
    :tail-matcher "`;"
    :head-mode 'host
    :tail-mode 'host)
  )


(provide 'init-polymode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-polymode.el ends here
