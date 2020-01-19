;;; Code:


;; nyan-mode
(use-package nyan-mode
  :init
  (nyan-mode t)
  :custom
  (nyan-animate-nyancat nil)
  (nyan-wavy-trail nil))

;; (setq
;;  evil-normal-state-tag   (propertize "< N >" 'face '((:background "green" :foreground "black")))
;;  evil-emacs-state-tag    (propertize "< E >" 'face '((:background "orange" :foreground "black")))
;;  evil-insert-state-tag   (propertize "< I >" 'face '((:background "red") :foreground "white"))
;;  evil-motion-state-tag   (propertize "< M >" 'face '((:background "blue") :foreground "white"))
;;  evil-visual-state-tag   (propertize "< V >" 'face '((:background "grey80" :foreground "black")))
;;  evil-operator-state-tag (propertize "< O >" 'face '((:background "purple"))))

(setq
 evil-normal-state-tag   (propertize "< N >" 'face 'font-lock-preprocessor-face)
 evil-emacs-state-tag    (propertize "< E >" 'face 'font-lock-preprocessor-face)
 evil-insert-state-tag   (propertize "< I >" 'face 'font-lock-preprocessor-face)
 evil-motion-state-tag   (propertize "< M >" 'face 'font-lock-preprocessor-face)
 evil-visual-state-tag   (propertize "< V >" 'face 'font-lock-preprocessor-face)
 evil-operator-state-tag (propertize "< O >" 'face 'font-lock-preprocessor-face))


;; ⓪ ① ② ③ ④ ⑤ ⑥ ⑦ ⑧ ⑨ ⑩
;; Ⅰ Ⅱ Ⅲ Ⅳ Ⅴ Ⅵ Ⅶ Ⅷ Ⅸ Ⅹ
(defun modeline-unicode-number (str)
  "Return a nice unicode representation of a single-digit number STR."
  (cond
   ((string= "1" str) " Ⅰ ")
   ((string= "2" str) " Ⅱ ")
   ((string= "3" str) " Ⅲ ")
   ((string= "4" str) " Ⅳ ")
   ((string= "5" str) " Ⅴ ")
   ((string= "6" str) " Ⅵ ")
   ((string= "7" str) " Ⅶ ")
   ((string= "8" str) " Ⅷ ")
   ((string= "9" str) " Ⅸ ")
   ((string= "0" str) " Ⅹ ")))


(defun modeline-fill (face reserve)
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
         (propertize (format " † %s  ‡ %s " (or no-warnings 0) (or no-errors 0))
                     'face face)))
      (`interrupted " -")
      (`suspicious '(propertize " ?" 'face 'warning)))))


(defun modeline-renderer ()
  "Mode line renderer."
  (let* ((modeline-left (list
                         ;; winum
                         " "
                         '(:eval (propertize
                                  (modeline-unicode-number (winum-get-number-string)) 'face 'font-lock-preprocessor-face))
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
                         "  "
                         
                         ;; evil state
                         '(:eval evil-mode-line-tag)
                         "   "
                         
                         ;; '(:eval (propertize "[" 'face 'font-lock-type-face))
                         '(:eval (list (nyan-create)))
                         ;; '(:eval (propertize "]" 'face 'font-lock-type-face))
                         "   "
                         
                         
                         ;; the current major mode for the buffer.
                         '(:eval (propertize "%m" 'face 'font-lock-string-face
                                             'help-echo buffer-file-coding-system))
                         "       "

                         ;; minor modes
                         ;; minor-mode-alist
                         ;; " "
                         
                         modeline-flycheck
                         " "
                         ))
         (modeline-middle (list
                           ;; insert vs overwrite mode, input-method in a tooltip
                           " "
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
                                     (concat ", "  (propertize "RO"
                                                              'face 'font-lock-type-face
                                                              'help-echo "Buffer is read-only"))))
                           " "
                           
                           ;; git info
                           '(:eval vc-mode)
                         ))
                        (modeline-right (list
                                         ;; global-mode-string goes in mode-line-misc-info
                                         mode-line-misc-info

                                         (modeline-fill 'mode-line 10)
                                         
                                         ;; line and column
                                         ;; '%02' to set to 2 chars at least; prevents flickering
                                         (propertize "%02l" 'face 'font-lock-type-face) ","
                                         (propertize "%02c" 'face 'font-lock-type-face)
                                         "  "
                                         
                                         '(:eval (buffer-encoding-abbrev))
                                         " "
                                         ))
                        (modeline-height (list
                                          (propertize " "
                                                      'display '(height 1.4))
                                          (propertize " " 'display '(raise -0.6))))

                        (width-left (string-width (format-mode-line modeline-left)))
                        (width-left-middle (string-width (format-mode-line (list modeline-left modeline-middle))))
                        (width-fill (string-width (format-mode-line (list modeline-left modeline-middle modeline-right)))))

    (cond
     ((> width-left (window-width)) (list modeline-left modeline-height))
     ((> width-left-middle (window-width)) (list modeline-left modeline-middle modeline-height))
     (t (list modeline-left modeline-middle modeline-right modeline-height)))))


(setq-default mode-line-format '(:eval (modeline-renderer)))
(set-face-attribute 'mode-line nil
                    :family "SF Pro Text"
                    :height 100
                    :overline nil
                    :underline nil)
(set-face-attribute 'mode-line-inactive  nil
                    :height 90
                    :overline nil
                    :underline nil
                    :background (face-background 'default))

(setq x-underline-at-descent-line t)

(provide 'init-modeline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-modeline.el ends here
