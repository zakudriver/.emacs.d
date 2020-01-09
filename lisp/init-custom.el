;;; Code:


(eval-when-compile
 (require 'init-const))


(defcustom kumo/package-archives 'emacs-china
  "Set package archives from which to fetch."
  :type '(choice
          (const :tag "Melpa" melpa)
          (const :tag "Emacs-China" emacs-china)
          (const :tag "Tuna" tuna)))

(defconst kumo/logo (expand-file-name "logo.png" user-emacs-directory))

(defconst kumo/default-theme 'monokai)

;; max: 18 themes
(defconst kumo/theme
  '((monokai-theme monokai)
    (dracula-theme dracula)
    (material-theme material)
    (material-theme material-light)
    (srcery-theme srcery)
    (flucui-themes flucui-dark)
    (flucui-themes flucui-light)
    (solarized-theme solarized-dark)
    (solarized-theme solarized-light)
    (lab-themes lab-dark)
    (lab-themes lab-light)
    ;; (doom-themes doom-cyberpunk)
    (nil tron-legacy))
    "Theme list.")


;; "/usr/local/go/bin" -- trigger --> go-mode lsp !!!
;; "/home/kumotyou/.config/yarn/global/node_modules" -- trigger --> ng2-mode lsp
(defconst kumo/env-path
  (if (and sys/macp)
    '("/usr/local/bin" "/Users/kumotyou/code/go/bin" "/usr/local/go/bin" "/Users/kumotyou/.config/yarn/global/node_modules")
    '("/home/kumotyou/Code/Go/bin" "/usr/lib/go/bin" "/home/kumotyou/.yarn/bin" "/home/kumotyou/.config/yarn/global/node_modules"))
  "ENV_PATH list.")



(provide 'init-custom)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
