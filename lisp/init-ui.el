;;; init-ui --- Summary

;;; Commentary:
;; some configuration of ui.

;;; Code:


(eval-when-compile
  (require 'init-const)
  (require 'init-custom)
  (require 'init-funcs))

;; Theme
(use-package lacquer
  :load-path "~/.emacs.d/site-lisp/lacquer"
  :pretty-hydra
  ((:title (pretty-hydra-title "Lacquer Management" 'faicon "th" :height 1 :v-adjust -0.1)
           :foreign-keys warn :quit-key ("q" "C-g"))
   ("Theme"
    (("t" lacquer-current-theme "current theme")
     ("T" lacquer-theme-selector "theme selector")
     ("C" lacquer-theme-carousel "theme carousel")
     ("M" lacquer-mode-selector "mode selector")
     ("a" lacquer-start-auto-switch "start auto switch")
     ("A" lacquer-stop-auto-switch "stop auto switch"))
    "Font"
    (("f" lacquer-current-font "current font")
     ("F" lacquer-font-selector "font selector"))))
  :hook
  (after-init . lacquer-auto-mode)
  :bind
  ("C-c l" . lacquer-hydra/body)
  :custom
  ;; (lacquer-auto-switch-time '("00:01" "00:02" "10:00" "14:00" "16:00" "18:00" "20:00" "22:00"))
  (lacquer-auto-switch-mode 'random)
  (lacquer-auto-switch-time (* 60 30))
  (lacquer-theme-list       my/theme-list)
  (lacquer-font-list        my/font-list))


;; Never lose your cursor again
;; (use-package beacon
;;   :hook
;;   (after-init . beacon-mode)
;;   :custom
;;   (beacon-blink-when-window-changes nil))


;; Font
(with-no-warnings
  (when sys/macp
    ;; (set-fontset-font
    ;;  t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
    (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
    
    
    (setq ns-use-thin-smoothing t
          ns-pop-up-frames nil)))


(set-face-attribute 'default nil :weight my/font-weight)
(setq-default line-spacing 0.3
              fill-column 80)


;; Title
(setq frame-title-format nil
      frame-resize-pixelwise t)


;; Transparent background
(modify-frame-parameters nil `((alpha . 85)))


;; Menu/Tool/Scroll bars
(when (version< emacs-version "27")
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)))
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))


;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))


;; Dashboard
(use-package dashboard
  :defines dashboard-recover-layout-p
  :bind
  (("C-c h" . my-open-dashboard)
   :map dashboard-mode-map
   ("C-, g" . dashboard-refresh-buffer)
   ("q"     . my-quit-dashboard))
  :custom-face
  (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  :custom
  ;; (dashboard-banner-logo-title "Help poor children in Uganda!")
  (dashboard-banner-logo-title      (concat "Happy hacking, " user-login-name " - Emacs ♥ you!"))
  (dashboard-set-file-icons         t)
  (dashboard-set-heading-icons      t)
  (dashboard-startup-banner         (or my/logo 'official))
  (dashboard-image-banner-max-width 200)
  (dashboard-center-content         t)
  (dashboard-show-shortcuts         nil)
  (dashboard-items                  '((recents  . 10)
                                      (projects . 10)))
  ;; (dashboard-init-info (concat "Happy hacking, " user-login-name " - Emacs ♥ you!"))
  (dashboard-set-footer nil)
  :custom-face
  (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  :hook
  (emacs-with-setup . (lambda ()
                        (forward-line my/dashboard-position)))
  :init
  (dashboard-setup-startup-hook)
  :config
  (defvar dashboard-recover-layout-p nil
    "Wether recovers the layout.")

  (defun my-open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    ;; Check if need to recover layout
    (when (length> (window-list-1) 1)
      (setq dashboard-recover-layout-p t)
      (my-save-window-configuration))
    ;; Display dashboard in maximized window
    (delete-other-windows)
    ;; Refresh dashboard buffer
    (dashboard-refresh-buffer)
    ;; Jump to the first section
    (let ((fn (local-key-binding "r")))
      (and fn (funcall fn))))

  (defun my-quit-dashboard ()
    "Quit dashboard window."
    (interactive)
    (quit-window t)
    (when dashboard-recover-layout-p
      (setq dashboard-recover-layout-p nil)
      (my-restore-window-configuration)))

  )


;; modeline nyan-mode
(use-package nyan-mode
  :load-path "~/.emacs.d/site-lisp/nyan-mode"
  :hook
  (after-init . nyan-mode)
  :custom
  (nyan-cat-flavor       'jazz)
  (nyan-bar-length       40)
  (nyan-animate-nyancat  nil)
  (nyan-wavy-trail       t)
  (nyan-animation-frames 10))


;; modeline parrot-mode
(use-package parrot
  :functions my-trigger-parrot
  :commands parrot-start-animation
  :hook
  (after-init . parrot-mode)
  :custom
  (parrot-num-rotations 1)
  :config
  (defun my-trigger-parrot (&_rest)
    "Trigger parrot animation."
    (parrot-start-animation))
  (if (boundp 'window-selection-change-functions)
      (add-hook 'window-selection-change-functions #'my-trigger-parrot)
    (add-hook 'post-command-hook #'my-trigger-parrot)))


;; mood-line-mode
(use-package mood-line
  :load-path "~/.emacs.d/site-lisp/mood-line"
  :hook
  (after-init . mood-line-mode)
  ;; :custom
  ;; (mood-line-background "#002FA7")
  ;; (mood-line-inactive-background "#5bc2e7")
  )


;; modeline poke-line
;; (use-package poke-line
;;   :hook
;;   (after-init . poke-line-global-mode)
;;   :custom
;;   (poke-line-bar-length 24)
;;   (poke-line-pokemon "gengar"))


(use-package fireplace
  :commands
  (fireplace-off my-restore-window-configuration my-save-window-configuration)
  :custom
  (fireplace-smoke-on t)
  :hook
  (after-init . my-switch-timing-fireplace)
  :config
  (defvar my/fireplace-timer nil
    "Fireplace timer.")
  (defvar my/fireplace-p nil
    "Wether recovers the layout.")
  
  (defun my-switch-timing-fireplace ()
    "Switch whether `fireplace be called regularly."
    (interactive)
    (if my/fireplace-timer
        (progn
          (cancel-timer my/fireplace-timer)
          (setq my/fireplace-timer nil))
      (setq my/fireplace-timer (run-with-idle-timer 500 t
						                                        (lambda ()
                                                      (fireplace)))))
    (message "The timing fireplace is %s." (if my/fireplace-timer "on" "off")))
  
  (advice-add #'fireplace-off :after (lambda ()
                                       (setq my/fireplace-p nil)
                                       (my-restore-window-configuration)))
  (advice-add #'fireplace :before (lambda ()
                                    (unless my/fireplace-p
                                      (setq my/fireplace-p t)
                                      (my-save-window-configuration)
                                      (delete-other-windows)))))


;; Emoji
(use-package emojify
  :if sys/linuxp
  :hook
  (after-init . (lambda ()
                  (if sys/linuxp
                      (global-emojify-mode)))))


;; Misc
;; (fset 'yes-or-no-p 'y-or-n-p) emacs28 to COMMENTS
(size-indication-mode t)
(setq inhibit-startup-screen            t
      use-file-dialog                   nil
      use-dialog-box                    nil
      inhibit-startup-echo-area-message t
      track-eol                         t
      use-short-answers                 t ;; emacs28
      line-move-visual                  nil)


;; Display dividers between windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)


;; Don't open a file in a new frame
(if (boundp 'ns-pop-up-frames)
    (setq ns-pop-up-frames nil))


;; Don't use GTK+ tooltip
(if (boundp 'x-gtk-use-system-tooltips)
    (setq x-gtk-use-system-tooltips nil))


;; icons
(use-package all-the-icons
  :commands my-font-installed-p
  :if
  (display-graphic-p)
  :init
  (unless (my-font-installed-p "all-the-icons")
    (all-the-icons-install-fonts t))
  :config
  (declare-function memoize 'memoize)
  (declare-function memoize-restore 'memoize)
  (defun all-the-icons-reset ()
    "Reset (unmemoize/memoize) the icons."
    (interactive)
    (ignore-errors
      (dolist (f '(all-the-icons-icon-for-file
                   all-the-icons-icon-for-mode
                   all-the-icons-icon-for-url
                   all-the-icons-icon-family-for-file
                   all-the-icons-icon-family-for-mode
                   all-the-icons-icon-family))
        (memoize-restore f)
        (memoize f)))
    (message "Reset all-the-icons"))

  (add-to-list 'all-the-icons-icon-alist
               '("^Rakefile$" all-the-icons-alltheicon "ruby-alt" :face all-the-icons-red))
  (add-to-list 'all-the-icons-icon-alist
               '("\\go.mod$" all-the-icons-fileicon "go" :face all-the-icons-dblue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\go.sum$" all-the-icons-fileicon "go" :face all-the-icons-dpurple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(xwidget-webkit-mode all-the-icons-faicon "chrome" :v-adjust -0.1 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(bongo-playlist-mode all-the-icons-material "queue_music" :height 1.2 :face 'all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(bongo-library-mode all-the-icons-material "library_music" :height 1.1 :face 'all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-group-mode all-the-icons-fileicon "gnu" :face 'all-the-icons-silver))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-summary-mode all-the-icons-octicon "inbox" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-article-mode all-the-icons-octicon "mail" :height 1.1 :v-adjust 0.0 :face 'all-the-icons-lblue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(message-mode all-the-icons-octicon "mail" :height 1.1 :v-adjust 0.0 :face 'all-the-icons-lblue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(diff-mode all-the-icons-octicon "git-compare" :v-adjust 0.0 :face all-the-icons-lred))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(flycheck-error-list-mode all-the-icons-octicon "checklist" :height 1.1 :v-adjust 0.0 :face all-the-icons-lred))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.rss$" all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(elfeed-search-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(elfeed-show-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-mode all-the-icons-faicon "rss-square" :v-adjust -0.1 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-list-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(newsticker-treeview-item-mode all-the-icons-octicon "rss" :height 1.1 :v-adjust 0.0 :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.[bB][iI][nN]$" all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.c?make$" all-the-icons-fileicon "gnu" :face all-the-icons-dorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.conf$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.toml$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(conf-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(conf-space-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(forge-topic-mode all-the-icons-alltheicon "git" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.xpm$" all-the-icons-octicon "file-media" :v-adjust 0.0 :face all-the-icons-dgreen))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(help-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(helpful-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(Info-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1))
  (add-to-list 'all-the-icons-icon-alist
               '("NEWS$" all-the-icons-faicon "newspaper-o" :height 0.9 :v-adjust -0.2))
  (add-to-list 'all-the-icons-icon-alist
               '("Cask\\'" all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(cask-mode all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '(".*\\.ipynb\\'" all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebooklist-mode all-the-icons-faicon "book" :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-multilang-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-dorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.epub\\'" all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(nov-mode all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gfm-mode all-the-icons-octicon "markdown" :face all-the-icons-lblue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.tsx$" all-the-icons-fileicon "typescript" :height 0.9 :face all-the-icons-dblue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(typescript-mode all-the-icons-fileicon "typescript" :height 0.9 :face all-the-icons-blue)))


(provide 'init-ui)

;;; init-ui.el ends here
