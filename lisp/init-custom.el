(eval-when-compile
 (require 'init-const))

(defcustom kumo-package-archives 'emacs-china
  "Set package archives from which to fetch."
  :type '(choice
          (const :tag "Melpa" melpa)
          (const :tag "Emacs-China" emacs-china)
          (const :tag "Tuna" tuna)))

(defcustom kumo-theme 'monokai
  "Set color theme."
  :type '(choice
          (const :tag "Doom theme" doom)
          (const :tag "Monokai theme" monokai)))

(defcustom use-yas nil
  "Enable yasnippet for company backends or not."
  :type 'boolean)

(provide 'init-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
