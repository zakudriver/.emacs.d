;;; Code:


(eval-when-compile
  (require 'init-const)
  (require 'init-custom))


;; Dashboard
(use-package dashboard
  :bind
  (:map dashboard-mode-map
        ("C-, g" . dashboard-refresh-buffer))
  :custom
  ;; (dashboard-banner-logo-title (concat "Happy hacking, " user-login-name " - Emacs ♥ you!"))
  (dashboard-banner-logo-title "Help poor children in Uganda!")
  (dashboard-set-file-icons t)
  (dashboard-set-heading-icons t)
  (dashboard-startup-banner (or kumo/logo 'official))
  (dashboard-center-content t)
  (dashboard-show-shortcuts nil)
  (dashboard-items '((recents  . 10)
                     (projects . 10)))
  (dashboard-init-info (concat "Happy hacking, " user-login-name " - Emacs ♥ you!"))
  (dashboard-set-footer nil)
  :init
  ;; init jump to (kumo/dashboard-position) line
  (add-hook 'emacs-startup-hook '(lambda ()
                                   (goto-line kumo/dashboard-position)))
  (dashboard-setup-startup-hook))


(defvar kumo/C-c-keybinds '(general-define-key
                            :prefix "C-c")
  "General <C>-<c> prefix key.")


;; Font
(defvar kumo/current-font-list (seq-filter
                                (apply-partially #'(lambda (font)
                                                     (if (find-font (font-spec :name (symbol-name font)))
                                                         t nil)
                                                     )) kumo/font-list)
  "Filter kumo/font-list by syetem fonts.")

(defun font-is-existing (target)
  "Check the font is exists and system font is exists.
TARGET is a symbol."
  (cl-loop for i in kumo/current-font-list
           when (and (eq i target) (find-font (font-spec :name (symbol-name target))))
           return t))

(defun set-default-font-cache ()
  "Write and return default-font."
  (write-region (symbol-name nil) nil kumo/font-setting-cache) nil)

(defun read-font-cache ()
  "Read font from font cache."
  (if (file-exists-p kumo/font-setting-cache)
      (let* ((string-list (split-string (with-temp-buffer
                                          (insert-file-contents kumo/font-setting-cache)
                                          (buffer-string)) ":" t))
             (font-list (list (intern (nth 0 string-list)) (string-to-number (nth 1 string-list)))))
        (if (font-is-existing (nth 0 font-list))
            font-list
          nil))
    nil))

(defun set-font-cache ()
  "Write font cache file."
  (write-region (format "%s:%d" (or kumo/current-font "") (or kumo/current-font-size kumo/default-font-size)) nil kumo/font-setting-cache))


;; set current font setting
(defvar kumo/current-font-setting (or (read-font-cache) nil))

;; current font family
(defvar kumo/current-font (or (nth 0 kumo/current-font-setting) nil))
;; current font size
(defvar kumo/current-font-size (or (nth 1 kumo/current-font-setting) kumo/default-font-size))


(set-face-attribute 'default nil :height kumo/current-font-size)

(defun font-func-factory (font)
  "Font function factory.
FONT is a symbol."
  (when (find-font (font-spec :name (symbol-name font)))
    `(defun ,font ()
       (interactive)
       (set-face-attribute 'default nil :font ,(symbol-name font))
       (setq kumo/current-font font)
       (set-font-cache))))

(defmacro font-func-macro-factory ()
  "Font function macro factory."
  `(progn ,@(mapcar 'font-func-factory kumo/current-font-list)))

(defun kumo-current-font ()
  "Current font."
  (interactive)
  (message "Current font: %s" (symbol-name kumo/current-font)))

(defun bind-change-font-keymap ()
  "Bind change font keymap on general."
  (let ((idx 1))
    (dolist (i kumo/current-font-list kumo/C-c-keybinds)
      (setq kumo/C-c-keybinds
            (append kumo/C-c-keybinds
                    `(,(concat "F" (if (> idx 9) (nth (- idx 10) kumo/index-map) (number-to-string idx))))
                    `((quote ,i))
                    ))
      (setq idx (+ idx 1)))

    (setq kumo/C-c-keybinds
          (append kumo/C-c-keybinds
                  '("F0" 'kumo-current-font)))))


;; create interactive font function
(when kumo/current-font-list
  (font-func-macro-factory))

;; init default font
(when kumo/current-font
  (funcall kumo/current-font))

;; bind change font keymap
(when kumo/current-font-list
  (bind-change-font-keymap))

(set-face-attribute 'default nil :weight kumo/font-weight)
(setq-default line-spacing 0.3
              fill-column 80)


;; Emoji
(use-package emojify
  :ensure nil
  :hook
  (after-init . (lambda ()
                  (when sys/linuxp
                    (global-emojify-mode)))))
(when sys/macp
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))


;; Title
(setq frame-title-format nil
      frame-resize-pixelwise t)


;; Menu/Tool/Scroll bars
(when (version< emacs-version "27")
  (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)))
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))


;; (defvar after-load-theme-hook nil
;;   "Hook run after a color theme is loaded using `load-theme'.")
;; (defadvice load-theme (after run-after-load-theme-hook activate)
;;   "Run `after-load-theme-hook'."
;;   (run-hooks 'after-load-theme-hook))


;; Color Theme
(defun set-default-theme ()
  "Write and return default-theme.
TARGET is a symbol."
  (write-region (symbol-name kumo/default-theme) nil kumo/theme-setting-cache) kumo/default-theme)

(defun theme-is-existing (target)
  "Check the theme is exists.
TARGET is theme mame."
  (cl-loop for i in kumo/theme-list
           when (eq (nth 1 i) target)
           return t))

(defun read-theme-cache ()
  "Read theme from theme cache."
  (if (file-exists-p kumo/theme-setting-cache)
      (let ((theme
             (intern (with-temp-buffer (insert-file-contents kumo/theme-setting-cache) (buffer-string)))))
        (if (theme-is-existing theme) theme (set-default-theme)))
    (set-default-theme)))

;; set current theme
(setq-default kumo/current-theme (or (read-theme-cache) kumo/default-theme))

(defmacro theme-factory-macro (name load-name &rest config)
  "Theme factory macro.
NAME is theme package name.
LOAD-NAME is theme name.
CONFIG is theme config."
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
       (setq kumo/current-theme (quote ,load-name)))))

(defun theme-func-factory (theme)
  "Theme function factory.
THEME is '(theme-package-name theme name)."
  `(defun ,(nth 1 theme) ()
     (interactive)
     (theme-factory-macro ,@theme)
     (write-region (symbol-name (quote ,(nth 1 theme))) nil kumo/theme-setting-cache)
     (update-modeline-format)))

(defmacro theme-func-macro-factory ()
  "Theme function macro factory."
  `(progn ,@(mapcar 'theme-func-factory kumo/theme-list)))

(defun kumo-current-theme ()
  "Current theme."
  (interactive)
  (message "Current theme: %s" (symbol-name kumo/current-theme)))

(defun bind-change-theme-keymap ()
  "Bind change theme keymap on general."
  (let ((idx 1))
    (dolist (i kumo/theme-list kumo/C-c-keybinds)
      (setq kumo/C-c-keybinds
            (append kumo/C-c-keybinds
                    `(,(concat "T" (if (> idx 9) (nth (- idx 10) kumo/index-map) (number-to-string idx))))
                    `((quote,(nth 1 i)))
                    ))
      (setq idx (+ idx 1)))
    (setq kumo/C-c-keybinds
          (append kumo/C-c-keybinds
                  '("T0" 'kumo-current-theme)))))

;; create interactive theme function
(theme-func-macro-factory)

;; init default theme
(funcall kumo/current-theme)

;; bind change theme keymap
(bind-change-theme-keymap)


;; Call Bind keymap ;;
(eval kumo/C-c-keybinds)
;;;;;;;;;;;;;;;;;;;;;;


;; Misc
(fset 'yes-or-no-p 'y-or-n-p)
(size-indication-mode t)
(setq inhibit-startup-screen t
      track-eol t
      line-move-visual nil )


;; Don't open a file in a new frame
(when (boundp 'ns-pop-up-frames)
  (setq ns-pop-up-frames nil))


;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))


;; icons
(use-package all-the-icons
  :init
  (unless (or sys/win32p (member "all-the-icons" (font-family-list)))
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
