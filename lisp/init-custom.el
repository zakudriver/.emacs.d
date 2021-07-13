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
                            )
  "Theme list.")


(defconst kumo/font-list
  '(Menlo
    Roboto\ Mono
    Anonymous\ Pro
    FantasqueSansMono
    FiraMono
    Fira\ Code
    Operator\ Mono
    Inconsolata
    Iosevka
    )
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


(defconst kumo/modeline-height 26
  "Modeline height.")


;; Hugo const
(defconst kumo/easy-hugo-basedir "~/WWW-BUILDER/"
  "Hugo basedir.")


(defconst kumo/easy-hugo-postdir "notebook-org"
  "Hugo postdir.")


(defconst kumo/easy-hugo-url "https://rx-78-kum0.github.io/"
  "Hugo site url.")


(defconst kumo/easy-hugo-preview-url "http://localhost:1313/"
  "Hugo preview url.")


(defconst kumo/easy-hugo-github-deploy-script "publish.sh"
  "Hugo github deploy script.")


(defconst kumo/easy-hugo-github-deploy-buffer-name "*Hugo Github Deploy*"
  "Hugo github deploy buffer name.")


(defconst kumo/lsp-major-mode '(go-mode ng2-html-mode sh-mode ruby-mode css-mode scss-mode sass-mode)
  "Lsp Supported major mode.")


(defconst kumo/lsp-on-save-major-mode '(ruby-mode)
  "Lsp Supported major mode.")


(defconst kumo/org-mode-export-html-css "/org/style.css"
  "Insert inline css file when org export html.")


(defconst kumo/perspective-state-file "/.perspective"
  "Perspective state cache file.")


(defconst kumo/evil-local-mode '(web-mode typescript-mode js2-mode go-mode ruby-mode css-mode scss-mode)
  "Start evil-local-mode list.")


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
