;;; Code:


(eval-when-compile
  (require 'init-const)
  (require 'init-custom))


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


(defvar kumo/current-theme (or (read-theme-cache) kumo/default-theme)
  "Current theme.")


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
     ;; (update-modeline-format)
     ))


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

(provide 'init-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-theme.el ends here
