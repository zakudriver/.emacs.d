;;; init-custom --- Summary

;;; Commentary:
;; somme customizable variables.

;;; Code:


(eval-when-compile
  (require 'init-const))


(defcustom kumo/package-archives-alist
  '((melpa    . (("gnu"    . "http://elpa.gnu.org/packages/")
                 ("nongnu" . "http://elpa.nongnu.org/nongnu/")
                 ("melpa"  . "http://melpa.org/packages/")))
    (emacs-cn . (("gnu"    . "http://1.15.88.122/gnu/")
                 ("nongnu" . "http://1.15.88.122/nongnu/")
                 ("melpa"  . "http://1.15.88.122/melpa/")))
    (bfsu     . (("gnu"    . "http://mirrors.bfsu.edu.cn/elpa/gnu/")
                 ("nongnu" . "http://mirrors.bfsu.edu.cn/elpa/nongnu/")
                 ("melpa"  . "http://mirrors.bfsu.edu.cn/elpa/melpa/")))
    (netease  . (("gnu"    . "http://mirrors.163.com/elpa/gnu/")
                 ("nongnu" . "http://mirrors.163.com/elpa/nongnu/")
                 ("melpa"  . "http://mirrors.163.com/elpa/melpa/")))
    (sjtu     . (("gnu"    . "http://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/")
                 ("nongnu" . "http://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/nongnu/")
                 ("melpa"  . "http://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/")))
    (tuna     . (("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                 ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                 ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
    (ustc     . (("gnu"    . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                 ("nongnu" . "http://mirrors.ustc.edu.cn/elpa/nongnu/")
                 ("melpa"  . "http://mirrors.ustc.edu.cn/elpa/melpa/"))))
  "A list of the package archives."
  :group 'centaur
  :type '(alist :key-type (symbol :tag "Archive group name")
                :value-type (alist :key-type (string :tag "Archive name")
                                   :value-type (string :tag "URL or directory name"))))


(defcustom kumo/package-archives 'emacs-cn
  "Set package archives from which to fetch."
  :group 'centaur
  :set (lambda (symbol value)
         (set symbol value)
         (setq package-archives
               (or (alist-get value kumo/package-archives-alist)
                   (error "Unknown package archives: `%s'" value))))
  :type `(choice ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                    kumo/package-archives-alist)))


;; (defconst kumo/package-archives 'tuna
;;   "Set package archives from which to fetch.
;; (choice
;;   (const :tag \"Melpa\" melpa)
;;   (const :tag \"Emacs-China\" emacs-china)
;;   (const :tag \"Tuna\" tuna)
;;   (const :tag \"Netease\" netease)
;;   (const :tag \"Melpa-Mirror\" melpa-mirror))")


(defconst kumo/logo (expand-file-name "logo.png" user-emacs-directory)
  "Path of logo file.")


(defconst kumo/default-theme 'monokai
  "Default theme.")


(defconst kumo/theme-list '((monokai-theme monokai)
                            (monokai-pro-theme monokai-pro)
                            (dracula-theme dracula)
                            (doom-themes doom-one-light)
                            (doom-themes doom-vibrant)
                            (doom-themes doom-nord)
                            (doom-themes doom-nord-light)
                            (doom-themes doom-palenight)
                            (doom-themes doom-tomorrow-night)
                            (doom-themes doom-spacegrey)
                            (doom-themes doom-fairy-floss)
                            (doom-themes doom-challenger-deep)
                            (doom-themes doom-sourcerer)
                            (doom-themes doom-solarized-dark)
                            (doom-themes doom-solarized-light)
                            (doom-themes doom-molokai)
                            (doom-themes doom-gruvbox)
                            (doom-themes doom-horizon)
                            (doom-themes doom-wilmersdorf)
                            (doom-themes doom-acario-dark)
                            (cyberpunk-theme cyberpunk)
                            (snazzy-theme snazzy)
                            (atom-one-dark-theme atom-one-dark)
                            (spacemacs-theme spacemacs-light)
                            (spacemacs-theme spacemacs-dark)
                            (srcery-theme srcery)
                            (kaolin-themes kaolin-dark)
                            (kaolin-themes kaolin-aurora)
                            (kaolin-themes kaolin-bubblegum)
                            (kaolin-themes kaolin-light)
                            (kaolin-themes kaolin-ocean)
                            (kaolin-themes kaolin-galaxy)
                            (kaolin-themes kaolin-eclipse)
                            (kaolin-themes kaolin-valley-dark)
                            (kaolin-themes kaolin-temple)
                            (leuven-theme leuven light (setq leuven-scale-outline-headlines nil))
                            (leuven-theme leuven-dark (setq leuven-scale-outline-headlines nil))
                            (ample-theme ample)
                            (ample-theme ample-flat)
                            (ample-theme ample-light)
                            (sublime-themes brin)
                            (sublime-themes granger)
                            (sublime-themes spolsky)
                            (sublime-themes fogus)
                            (sublime-themes junio)
                            (jazz-theme jazz)
                            (grandshell-theme grandshell)
                            (tron-legacy-theme tron-legacy)
                            (afternoon-theme afternoon))
  "Theme list.")


(defconst kumo/font-list
  '(Menlo
    SF\ Mono
    Monaco
    Roboto\ Mono
    Ubuntu\ Mono
    Anonymous\ Pro
    FantasqueSansMono
    FiraMono
    Fira\ Code
    Operator\ Mono
    Inconsolata
    Iosevka
    Cochin
    Optima
    Victor\ Mono
    JetBrains\ Mono)
  "Font list.")


(defconst kumo/default-font-size
  (if sys/macp 135 100)
  "Font size.")


(defconst kumo/font-weight
  'normal
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
                    :resourceDir "/Library/Developer/CommandLineTools/usr/lib/clang/11.0.3"))
    nil)
  "CCLS initialization options.")


(defconst kumo/easy-hugo-basedir "~/WWW-BUILDER/"
  "Hugo basedir.")


(defconst kumo/easy-hugo-postdir "notebook-org"
  "Hugo postdir.")


(defconst kumo/easy-hugo-url "https://zakudriver.github.io/"
  "Hugo site url.")


(defconst kumo/easy-hugo-preview-url "http://localhost:1313/"
  "Hugo preview url.")


(defconst kumo/easy-hugo-github-deploy-script "publish.sh"
  "Hugo github deploy script.")


(defconst kumo/easy-hugo-github-deploy-buffer-name "*Hugo Github Deploy*"
  "Hugo github deploy buffer name.")


(defconst kumo/lsp-major-mode '(go-mode ng2-html-mode sh-mode ruby-mode css-mode scss-mode sass-mode web-mode typescript-mode js-mode clojure-mode dart-mode)
  "Lsp Supported major mode.")


(defconst kumo/lsp-on-save-major-mode '(ruby-mode clojure-mode dart-mode)
  "Lsp Supported major mode.")


(defconst kumo/org-mode-export-html-css "/org/style.css"
  "Insert inline css file when org export html.")


(defconst kumo/perspective-state-file "/.perspective"
  "Perspective state cache file.")


(defconst kumo/evil-local-mode '(web-mode typescript-mode js-mode js-mode go-mode ruby-mode css-mode scss-mode)
  "Start evil-local-mode list.")


(defconst kumo/flycheck-boot-mode '(not text-mode outline-mode fundamental-mode lisp-interaction-mode
                                        org-mode diff-mode shell-mode eshell-mode term-mode vterm-mode)
  "Start flycheck list.")


(defconst kumo/global-nodemodules-path "/.pnpm-global/5/node_modules"
  "Global node_modules path.")


(defconst kumo/native-compile-async-jobs (or (ignore-errors
	                                             (string-to-number (shell-command-to-string "nproc")))
	                                           4)
  "How many jobs to use.")


(defconst kumo/org-headline-bullets-list '("🌞" "🌤" "⛈" "🌈")
  "List of org headline bullets.")


;; (defconst kumo/prettify-symbols-alist
;;   '(("lambda" . ?λ)
;;     ("<-" . ?←)
;;     ("->" . ?→)
;;     ("->>" . ?↠)
;;     ("=>" . ?⇒)
;;     ("map" . ?↦)
;;     ("/=" . ?≠)
;;     ("!=" . ?≠)
;;     ("==" . ?≡)
;;     ("<=" . ?≤)
;;     (">=" . ?≥)
;;     ("<=<" . ?↢)
;;     (">=>" . ?↣))
;;   "Alist of symbol prettifications.")


(provide 'init-custom)

;;; init-custom.el ends here
