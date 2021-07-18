;;; Code:


(eval-when-compile
  (require 'cl)
  (require 'init-const)
  (require 'init-custom))


(with-no-warnings
  (when sys/macp
    (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
    (setq ns-use-thin-smoothing t
          ns-pop-up-frames nil)))


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


(defun read-font-cache ()
  "Read font from font cache."
  (when (file-exists-p kumo/font-setting-cache)
    (let ((cache (with-temp-buffer
                   (insert-file-contents kumo/font-setting-cache)
                   (buffer-string))))
      (when cache
        (let ((string-list (split-string cache ":" t)))
          (when (= (length string-list) 2)
            (let ((font-list (list (intern (nth 0 string-list)) (string-to-number (nth 1 string-list)))))
              (if (font-is-existing (nth 0 font-list))
                  font-list
                nil)))
          )))))


(defvar kumo/current-font-setting (or (read-font-cache) nil)
  "Current font setting.")


(defvar kumo/current-font (or (nth 0 kumo/current-font-setting) nil)
  "Current font family.")


(defvar kumo/current-font-size (or (nth 1 kumo/current-font-setting) kumo/default-font-size)
  "Current font size.")


(defun set-font-cache (&optional font-name font-size)
  "Write font cache file.
FONT-NAME: symbol.
FONT-SIZE: number."
  (when font-name
    (setq kumo/current-font font-name))
  (when font-size
    (setq kumo/current-font-size font-size))
  (write-region
   (format "%s:%d"
           (symbol-name kumo/current-font)
           kumo/current-font-size) nil kumo/font-setting-cache))


(defun font-func-factory (font)
  "Font function factory.
FONT is a symbol."
  (when (find-font (font-spec :name (symbol-name font)))
    `(defun ,font ()
       (interactive)
       (set-face-attribute 'default nil :font ,(symbol-name font))
       (set-font-cache (quote ,font)))))


(defmacro font-func-macro-factory ()
  "Font function macro factory."
  `(progn ,@(mapcar 'font-func-factory kumo/current-font-list)))


(defun kumo-current-font ()
  "Current font."
  (interactive)
  (message "Current font: %s" (symbol-name kumo/current-font)))


(defun bind-change-font-keymap ()
  "Bind change font keymap on general."
  (let ((i 1))
    (dolist (v kumo/current-font-list)
      (global-set-key (kbd
                       (concat "C-c F " (if (> i 9) (nth (- i 10) kumo/expanding-index-map) (number-to-string i))))
                      v)
      (incf i)
      )))


;; create interactive font function
(when kumo/current-font-list
  (font-func-macro-factory))


;; init default font
(when kumo/current-font
  (funcall kumo/current-font))


;; bind change font keymap
(when kumo/current-font-list
  (bind-change-font-keymap))


;; size, weight and line-space 
(set-face-attribute 'default nil :height kumo/current-font-size)
(set-face-attribute 'default nil :weight kumo/font-weight)
(setq-default line-spacing 0.3
              fill-column 80)


(provide 'init-font)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-font.el ends here
