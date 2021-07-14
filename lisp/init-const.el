;;; Code:


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
                             ("<" . ">"))
  "Symbols for wrap.")


(defconst kumo/theme-setting-cache
  "~/.emacs.d/.theme"
  "Theme setting cache.")


(defconst kumo/font-setting-cache
  "~/.emacs.d/.font"
  "Font setting cache.")


(defconst kumo/env-path-file
  "~/.emacs.d/.PATH"
  "Env path file.")


(defconst kumo/expanding-index-map
  '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z 1" "z 2" "z 3" "z 4" "z 5" "z 6" "z 7" "z 8" "z 9")
  "When index > 9.")


(defconst kumo/flycheck-errors-buffer-name "*Flycheck errors*"
  "Flycheck errors buffer name.")


(defconst kumo/dashboard-position 8
  "Dashboard init cursor position.")


(provide 'init-const)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-const.el ends here
