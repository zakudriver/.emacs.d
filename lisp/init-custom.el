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

(defconst kumo/theme
  '((monokai-theme monokai)
    (dracula-theme dracula)
    (srcery-theme srcery)
    (lab-themes lab-dark)
    (doom-themes doom-vibrant)
    (doom-themes doom-city-lights)
    (doom-themes doom-nord)
    (doom-themes doom-oceanic-next)
    (doom-themes doom-palenight)
    (doom-themes doom-tomorrow-night)
    (doom-themes doom-spacegrey)
    (doom-themes doom-fairy-floss)
    (doom-themes doom-snazzy)
    (doom-themes doom-challenger-deep)
    (doom-themes doom-sourcerer)
    (doom-themes doom-solarized-dark)
    (doom-themes doom-solarized-light)
    (cyberpunk-theme cyberpunk)
    (nil tron-legacy))
  "Theme list.")


;; "/usr/local/go/bin" -- trigger --> go-mode lsp !!!
;; "/home/kumotyou/.config/yarn/global/node_modules" -- trigger --> ng2-mode lsp
(defconst kumo/env-path
  (if (and sys/macp)
    '("/usr/local/bin" "/Users/kumotyou/code/go/bin" "/Users/kumotyou/code/go" "/usr/local/go/bin" "/Users/kumotyou/.config/yarn/global/node_modules")
    '("/home/kumotyou/Code/Go/bin" "/home/kumotyou/Code/Go" "/usr/lib/go/bin" "/home/kumotyou/.yarn/bin" "/home/kumotyou/.config/yarn/global/node_modules"))
  "ENV_PATH list.")



(provide 'init-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
