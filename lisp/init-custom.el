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


(defconst kumo/theme '((monokai-theme monokai)
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
                       (nil tron-legacy)
                       (snazzy-theme snazzy)
                       (atom-one-dark-theme atom-one-dark))
  "Theme list.")


(defconst kumo/font
  (if sys/macp
      "Menlo"
    "Anonymous Pro")
  "Font.")


(defconst kumo/font-size
  (if sys/macp 130 105)
  "Font size.")


(defconst kumo/font-weight
  'bold
  "Font weight.")


(defconst kumo/env-path
  (if (file-exists-p kumo/env-path-file)
      (split-string (with-temp-buffer
                      (insert-file-contents kumo/env-path-file)
                      (buffer-string)) ":" t)
    (write-region "" nil kumo/env-path-file)
    nil)
  "ENV_PATH list.")


(defconst kumo/ccls-initialization-options
  (if sys/macp
      '(:clang
        (:extraArgs ["-isystem/Library/Developer/CommandLineTools/usr/include/c++/v1"
                     "-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
                     "-isystem/usr/local/include"]
                    :resourceDir "/Library/Developer/CommandLineTools/usr/lib/clang/11.0.0"))
    nil)
  "CCLS initialization options.")


(provide 'init-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
