;;; init-consts --- Summary

;;; Commentary:
;; some consts.

;;; Code:


(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")


(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")


(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")


(defconst my/username
  (getenv "USER")
  "Username.")


(defconst my/symbol-list '(
                             ("(" . ")")
                             ("[" . "]")
                             ("{" . "}")
                             ("<" . ">"))
  "Symbols for wrap.")


(defconst my/font-setting-cache
  "~/.emacs.d/.font"
  "Font setting cache.")


(defconst my/env-path-file
  "~/.emacs.d/.PATH"
  "Env path file.")


(defconst my/expanding-index-map
  '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z 1" "z 2" "z 3" "z 4" "z 5" "z 6" "z 7" "z 8" "z 9")
  "When index > 9.")


(defconst my/flycheck-errors-buffer-name "*Flycheck errors*"
  "Flycheck errors buffer name.")


(defconst my/dashboard-position 8
  "Dashboard init cursor position.")


(defconst my/left-brackets '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" "\"")
  "List of left bracket chars.")


(defconst my/right-brackets '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»" "\"")
  "List of right bracket chars.")


(provide 'init-const)

;;; init-const.el ends here
