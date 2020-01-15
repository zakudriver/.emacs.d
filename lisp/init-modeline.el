;;; Code:


;; nyan-mode
(use-package nyan-mode
  :init
  (nyan-mode t)
  :custom
  (nyan-animate-nyancat nil)
  (nyan-wavy-trail nil))

(setq
 evil-normal-state-tag   (propertize "[N]" 'face '((:background "green" :foreground "black")))
 evil-emacs-state-tag    (propertize "[E]" 'face '((:background "orange" :foreground "black")))
 evil-insert-state-tag   (propertize "[I]" 'face '((:background "red") :foreground "white"))
 evil-motion-state-tag   (propertize "[M]" 'face '((:background "blue") :foreground "white"))
 evil-visual-state-tag   (propertize "[V]" 'face '((:background "grey80" :foreground "black")))
 evil-operator-state-tag (propertize "[O]" 'face '((:background "purple"))))


(defun modeline-unicode-number (str)
  "Return a nice unicode representation of a single-digit number STR."
  (cond
   ((string= "1" str) "➊")
   ((string= "2" str) "➋")
   ((string= "3" str) "➌")
   ((string= "4" str) "➍")
   ((string= "5" str) "➎")
   ((string= "6" str) "➏")
   ((string= "7" str) "➐")
   ((string= "8" str) "➑")
   ((string= "9" str) "➒")
   ((string= "0" str) "➓")))


(defun mode-line-fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to
                                (- (+ right right-fringe right-margin) ,reserve)))
              'face face))

(defun buffer-encoding-abbrev ()
  "The line ending convention used in the buffer."
  (let ((buf-coding (format "%s" buffer-file-coding-system)))
    (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
        (match-string 1 buf-coding)
      buf-coding)))

;; flycheck
(defvar modeline-flycheck
  '(:eval
    (pcase flycheck-last-status-change
      (`not-checked nil)
      (`no-checker (propertize " -" 'face 'warning))
      (`running (propertize " ✷" 'face 'success))
      (`errored (propertize " !" 'face 'error))
      (`finished
       (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
              (no-errors (cdr (assq 'error error-counts)))
              (no-warnings (cdr (assq 'warning error-counts)))
              (face (cond (no-errors 'error)
                          (no-warnings 'warning)
                          (t 'success))))
         (propertize (format "%s / %s" (or no-errors 0) (or no-warnings 0))
                     'face face)))
      (`interrupted " -")
      (`suspicious '(propertize " ?" 'face 'warning)))))


(setq-default mode-line-format
              (list
               ;; winum
               " "
               '(:eval (propertize
                        (modeline-unicode-number (winum-get-number-string))))
               " "

               ;; is Modified
               '(:eval (propertize "%*" 'face 'font-lock-string-face))
               " "

               ;; file info
               ;; " %* "
               '(:eval (propertize
                        (modeline-window-number)
                        'face
                        'font-lock-type-face))

               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                                   'help-echo (buffer-file-name)))

               ;; size
               '(:eval (propertize "%I" 'face 'font-lock-constant-face))
               " "

               ;; evil state
               '(:eval evil-mode-line-tag)
               " "

               "["
               '(:eval (list (nyan-create)))
               "] "
               
               ;; insert vs overwrite mode, input-method in a tooltip
               "["
               '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                                   'face 'font-lock-preprocessor-face
                                   'help-echo (concat "Buffer is in "
                                                      (if overwrite-mode
                                                          "overwrite"
                                                        "insert") " mode")))
               
               ;; was this buffer modified since the last save?
               ;; '(:eval (when (buffer-modified-p)
               ;;           (concat ","  (propertize "Mod"
               ;;                                    'face 'font-lock-warning-face
               ;;                                    'help-echo "Buffer has been modified"))))
               
               ;; is this buffer read-only?
               '(:eval (when buffer-read-only
                         (concat ","  (propertize "RO"
                                                  'face 'font-lock-type-face
                                                  'help-echo "Buffer is read-only"))))
               "] "
               
               
               ;; the current major mode for the buffer.
               '(:eval (propertize "%m" 'face 'font-lock-string-face
                                   'help-echo buffer-file-coding-system))
               "  "
               
               modeline-flycheck
               " "
               
               ;; minor modes
               ;; minor-mode-alist
               ;; " "

               ;; git info
               '(:eval vc-mode)
               " "
               
               ;; global-mode-string goes in mode-line-misc-info
               mode-line-misc-info
               
               (mode-line-fill 'mode-line 20)
               
               ;; line and column
               "(" ;; '%02' to set to 2 chars at least; prevents flickering
               (propertize "%02l" 'face 'font-lock-type-face) ","
               (propertize "%02c" 'face 'font-lock-type-face)
               ") "
               
               '(:eval (buffer-encoding-abbrev))
               mode-line-end-spaces

               (propertize " "
                           'display '(height 1.3))
               (propertize " " 'display '(raise -0.1))
               ))


(setq x-underline-at-descent-line t)

(provide 'init-modeline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-modeline.el ends here
