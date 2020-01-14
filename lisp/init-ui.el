;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Font
(set-face-attribute 'default nil :height 120)
;; (set-face-attribute 'default nil :font "Menlo")


;;;;;;;;;;;;;;;;
;; Dashboard
;;;;;;;;;;;;;;;;
(use-package dashboard
  :ensure t
  :custom
  (dashboard-banner-logo-title (concat "Happy hacking, " user-login-name " - Emacs ♥ you!"))
  (dashboard-set-file-icons t)
  (dashboard-set-heading-icons t)
  (dashboard-startup-banner (or kumo/logo 'official))
  (dashboard-center-content t)
  (dashboard-show-shortcuts nil)
  ( dashboard-items '((recents  . 5)
                      (projects . 5)))
  (dashboard-set-footer nil)
  :init
  (dashboard-setup-startup-hook))



;; Initial scratch message
(setq-default initial-scratch-message
              (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))
(setq inhibit-startup-message t)


;; Title
(setq frame-title-format
      '("Emacs " emacs-version "@" user-login-name " : "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(setq icon-title-format frame-title-format)


;; Menu/Tool/Scroll bars
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))


;; (defvar after-load-theme-hook nil
;;   "Hook run after a color theme is loaded using `load-theme'.")
;; (defadvice load-theme (after run-after-load-theme-hook activate)
;;   "Run `after-load-theme-hook'."
;;   (run-hooks 'after-load-theme-hook))


;;;;;;;;;;;;;;;;
;; Color Theme
;;;;;;;;;;;;;;;;
(defun set-default-theme ()
  "Write and return default-theme"
  (progn
      (write-region (symbol-name kumo/default-theme) nil kumo/theme-setting-cache) kumo/default-theme))

(defun theme-is-existing (target)
  "Check the theme is exists"
  (cl-loop for i in kumo/theme
        when (eq (nth 1 i) target)
        return t))

(defun read-theme-cache ()
  "Read theme for theme cache"
  (if (file-exists-p kumo/theme-setting-cache)
      (let ((theme
             (intern (with-temp-buffer (insert-file-contents kumo/theme-setting-cache) (buffer-string))))) 
        (if (theme-is-existing theme) theme (set-default-theme))) 
    (set-default-theme)))


;; set current theme
(setq kumo/current-theme (read-theme-cache))

;; theme factory macro
(defmacro theme-factory-macro (name load-name &rest config)
  "Theme factory macro."
  (if name
      `(use-package ,name
         :init
         (disable-theme kumo/current-theme)
         (load-theme (quote ,load-name) t)
         (setq kumo/current-theme (quote ,load-name))
         ,@config)
    `(progn
      (disable-theme kumo/current-theme)
      (load-theme (quote ,load-name) t)
      (setq kumo/current-theme (quote ,load-name))))
)


(defun create-theme-func (theme)
  `(defun ,(nth 1 theme) ()
     (interactive)
     (theme-factory-macro ,@theme)
     (write-region (symbol-name (quote ,(nth 1 theme))) nil kumo/theme-setting-cache)
     ))


(defmacro create-theme-func-macro ()
  `(progn ,@(mapcar 'create-theme-func kumo/theme)))


;; bind theme keymap. 
(defun bind-change-theme-keymap ()
  "Bind change theme keymap on general."
  (eval
   (let ((keybinds '(general-define-key
              :prefix "C-c"))
        (idx 0))
    (dolist (i kumo/theme keybinds)
               (setq keybinds (append keybinds `(,(concat "t" (if (> idx 9) (nth (- idx 10) kumo/index-map) (number-to-string idx)))) `((quote ,(nth 1 i)))))
               (setq idx (+ idx 1))))))


;; create-interactive-theme-func
(create-theme-func-macro)

;; init default theme
(funcall kumo/current-theme)

;; bind change theme keymap
(bind-change-theme-keymap)


;;;;;;;;;;;;;;;;
;; Mode Line
;;;;;;;;;;;;;;;;
;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode)
;;   :init
;;   (setq doom-modeline-height 1)
;;   (set-face-attribute 'mode-line nil :height 110)
;;   (set-face-attribute 'mode-line-inactive nil :height 110)
;;   :custom
;;   (doom-modeline-buffer-file-name-style 'file-name)
;;   (doom-modeline-icon (display-graphic-p))
;;   (doom-modeline-major-mode-icon nil))


;; (use-package minions
;;   :ensure t
;;   :hook
;;   (after-init . minions-mode)
;;   :config (minions-mode t))



;; Line and Column
;;(setq-default fill-column 80)
;;(setq column-number-mode t)

(use-package smooth-scrolling
  :init (add-hook 'after-init-hook #'smooth-scrolling-mode)
  :config (setq smooth-scroll-margin 0
                scroll-conservatively 100000
                scroll-preserve-screen-position 1))


;; Misc
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default fill-column 80)
(size-indication-mode t)
(setq inhibit-startup-screen t)
(setq track-eol t)
(setq line-move-visual nil)




;; Don't open a file in a new frame
(when (boundp 'ns-pop-up-frames)
  (setq ns-pop-up-frames nil))


;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))



;;;;;;;;;;;;;;;;
;; Highlight
;;;;;;;;;;;;;;;;
;; line highlight
(global-hl-line-mode t)

;; paren highlight
(show-paren-mode 1)
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
	(t (save-excursion
	     (ignore-errors (backward-up-list))
	     (funcall fn)))))


;;;;;;;;;;;;;;;;
;; icons
;;;;;;;;;;;;;;;;
(use-package all-the-icons
  :if (display-graphic-p)
  :init (unless (or sys/win32p (member "all-the-icons" (font-family-list)))
          (all-the-icons-install-fonts t))
  :config
  (add-to-list 'all-the-icons-mode-icon-alist
               '(vterm-mode all-the-icons-octicon "terminal" :v-adjust 0.2))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.xpm$" all-the-icons-octicon "file-media" :v-adjust 0.0 :face all-the-icons-dgreen))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.lua$" all-the-icons-fileicon "lua" :face all-the-icons-dblue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(lua-mode all-the-icons-fileicon "lua" :face all-the-icons-dblue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.go$" all-the-icons-fileicon "go" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(go-mode all-the-icons-fileicon "go" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.tsx$" all-the-icons-fileicon "typescript" :height 0.9 :face all-the-icons-dblue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.tx$" all-the-icons-fileicon "typescript" :height 0.9 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(typescript-mode all-the-icons-fileicon "typescript" :height 0.9 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(help-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
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
               '(ein:notebooklist-mode all-the-icons-faicon "book" :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-multilang-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.epub\\'" all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(nov-mode all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gfm-mode all-the-icons-octicon "markdown" :face all-the-icons-blue)))




(provide 'init-ui)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
