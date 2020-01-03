;;; Code:


(eval-when-compile
 (require 'init-const))


(defcustom kumo/package-archives 'emacs-china
  "Set package archives from which to fetch."
  :type '(choice
          (const :tag "Melpa" melpa)
          (const :tag "Emacs-China" emacs-china)
          (const :tag "Tuna" tuna)))


(defconst kumo/default-theme 'flucui-dark)

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
    )
    "Theme list.")


(defcustom use-yas nil
  "Enable yasnippet for company backends or not."
  :type 'boolean)


 ;; "/usr/local/go/bin" -- complete --> go mode lsp !!!
(defconst kumo/env-path
  (if (and sys/macp)
    '("/usr/local/bin" "/Users/kumotyou/code/golang/bin" "/Users/kumotyou/code/go/bin" "/usr/local/go/bin")
    '("/home/kumotyou/Code/Go/bin" "/usr/lib/go/bin" "/home/kumotyou/.yarn/bin"))
  "ENV_PATH list.")



(provide 'init-custom)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
