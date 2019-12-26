;;; Code:


(eval-when-compile
 (require 'init-const))


(defcustom kumo-package-archives 'emacs-china
  "Set package archives from which to fetch."
  :type '(choice
          (const :tag "Melpa" melpa)
          (const :tag "Emacs-China" emacs-china)
          (const :tag "Tuna" tuna)))

(defcustom kumo-theme 'dracula
  "Set color theme."
  :type '(choice
          (const :tag "Doom theme" doom)
          (const :tag "Sanityinc theme" dracula)
          (const :tag "Monokai theme" monokai)))

(defcustom use-yas nil
  "Enable yasnippet for company backends or not."
  :type 'boolean)

(defconst kumo-path
  (if (and sys/macp)
    '("/usr/local/bin" "/Users/kumotyou/code/golang/bin" "/Users/kumotyou/code/go/bin")
    '("/home/kumotyou/Code/Go/bin" "/usr/lib/go/bin"))
  "PATH list.")



(provide 'init-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
