(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst kumo/username
  (getenv "USER")
  "Username.")


(defconst kumo/symbol-list '(
                    ("(" . ")")
                    ("[" . "]")
                    ("{" . "}")
                    ("<" . ">")
                    )
  "Symbols for wrap ")

(defconst kumo/theme-setting-cache
  "~/.emacs.d/.theme"
  "theme-setting-cache")


(provide 'init-const)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-const.el ends here
