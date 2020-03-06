;;; Code:


(eval-when-compile
  (require 'init-const)
  (require 'init-custom))


;; nyan-mode
(use-package nyan-mode
  :hook
  (after-init . nyan-mode)
  :custom
  (nyan-animate-nyancat nil)
  (nyan-wavy-trail nil)
  (nyan-animation-frame-interval 0.4))


;; (setq evil-normal-state-tag   (propertize "<𝙽>" 'face 'font-lock-preprocessor-face)
;;       evil-emacs-state-tag    (propertize "<𝙴>" 'face 'font-lock-preprocessor-face)
;;       evil-insert-state-tag   (propertize "<𝙸>" 'face 'font-lock-preprocessor-face)
;;       evil-motion-state-tag   (propertize "<𝙼>" 'face 'font-lock-preprocessor-face)
;;       evil-visual-state-tag   (propertize "<𝚅>" 'face 'font-lock-preprocessor-face)
;;       evil-operator-state-tag (propertize "<𝙾>" 'face 'font-lock-preprocessor-face))


(defun modeline-modified-p ()
  "Buffer modified symbol."
  (if (buffer-modified-p)
      "●"
    " "))


;; ⓪ ① ② ③ ④ ⑤ ⑥ ⑦ ⑧ ⑨ ⑩
;; Ⅰ Ⅱ Ⅲ Ⅳ Ⅴ Ⅵ Ⅶ Ⅷ Ⅸ Ⅹ
;; ०	१	२	३	४	५	६	७	८	९
(defun modeline-unicode-number (str)
  "Return a nice unicode representation of a single-digit number STR."
  (cond
   ((string= "1" str) "१")
   ((string= "2" str) "२")
   ((string= "3" str) "३")
   ((string= "4" str) "४")
   ((string= "5" str) "५")
   ((string= "6" str) "६")
   ((string= "7" str) "७")
   ((string= "8" str) "८")
   ((string= "9" str) "९")
   ((string= "0" str) "०")))


(defun modeline-fill (reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  ;; (when (and window-system (eq 'right (get-scroll-bar-mode)))
  ;;   (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to
                                (- (+ right right-fringe right-margin) ,reserve)))))


(defun modeline-buffer-encoding-abbrev ()
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

         (propertize (format " † %s ‡ %s " (or no-warnings 0) (or no-errors 0))
                     'face `(,face (:weight bold)))))
      (`interrupted " -")
      (`suspicious '(propertize " ?" 'face 'warning)))))


;; vc-mode
(defun modeline-vc-branch ()
  "Git branch."
  (if vc-mode
      (let* ((noback (replace-regexp-in-string (format "^ %s" (vc-backend buffer-file-name)) " " vc-mode))
             (face (cond ((string-match "^ -" noback) 'mode-line-vc)
                         ((string-match "^ [:@]" noback) 'mode-line-vc-edit)
                         ((string-match "^ [!\\?]" noback) 'mode-line-vc-modified))))
        (format " %s" (substring noback 2))))
  ;; (let ((backend (vc-backend buffer-file-name)))
  ;;   (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2)))
  )


(defun modeline-git-status ()
  "Git status."
  (let ((U 0)   ; untracked files
        (M 0)   ; modified files

        (O 0)   ; other files
        (U-files "")
        (M-files "")
        (O-files ""))
    (dolist (line (split-string
                   (shell-command-to-string "git status --porcelain")
                   "\n"))
      (cond
       ;; ignore empty line at end
       ((string= "" line) nil)

       ((string-match "^\\?\\?" line)
        (setq U (+ 1 U))
        (setq U-files (concat U-files "\n" line)))

       ((string-match "^ M" line)
        (setq M (+ 1 M))
        (setq M-files (concat M-files "\n" line))
        )

       (t
        (setq O (+ 1 O))
        (setq O-files (concat O-files "\n" line)))))
    
    ;; construct propertized string
    (format " %d / %d / %d " M U O)))


(defun modeline-renderer ()
  "Mode line renderer."
  (let* (
         (modeline-left (list
                         "  "
                         ;; winum
                         (propertize
                          (modeline-unicode-number (winum-get-number-string))
                          'face 'font-lock-preprocessor-face)
                         " "

                         ;; is Modified
                         ;; (propertize "%*" 'face 'font-lock-string-face)
                         (propertize (modeline-modified-p) 'face 'font-lock-string-face)
                         " "
                         
                         ;; the buffer name; the file name as a tool tip
                         (propertize "%b " 'face '(font-lock-keyword-face (:weight bold))
                                     'help-echo (buffer-file-name))
                         " "
                         
                         ;; size
                         (propertize "%I" 'face 'font-lock-constant-face)
                         "    "
                         
                         ;; evil state
                         '(:eval evil-mode-line-tag)

                         ;; modeline padding.
                         (propertize " "
                                     'display '(height 1.4))
                         (propertize " " 'display '(raise -0.6))
                         " "
                         
                         ;; nayan cat
                         '(:eval (nyan-create))
                         "    "
                         
                         ;; the current major mode for the buffer.
                         (propertize "%m"
                                     'face '(font-lock-string-face (:weight bold))
                                     'help-echo buffer-file-coding-system)
                         "     "
                         modeline-flycheck

                         "     "
                         ;; git info
                         ;; (propertize ,`(vc-mode vc-mode) 'face 'font-lock-keyword-face)
                         (propertize
                          (modeline-git-status) 'face '(font-lock-string-face (:weight bold)))


                         ;;                (propertize " " 'display
                         ;;                            '(image :type xpm :data "/* XPM */
                         ;; static char * close_tab_xpm[] = {
                         ;; \"14 11 3 1\",
                         ;; \"       c None\",
                         ;; \".      c #000000\",
                         ;; \"+      c #FFFFFF\",
                         ;; \"     .....    \",
                         ;; \"    .......   \",
                         ;; \"   .........  \",
                         ;; \"  ...+...+... \",
                         ;; \"  ....+.+.... \",
                         ;; \"  .....+..... \",
                         ;; \"  ....+.+.... \",
                         ;; \"  ...+...+... \",
                         ;; \"   .........  \",
                         ;; \"    .......   \",
                         ;; \"     .....    \"};" :ascent center :mask (heuristic t) :margin 0))

                         ))
         (modeline-right (list
                          (modeline-fill (if sys/macp 12 16))
                          ;; global-mode-string goes in mode-line-misc-info
                          ;; mode-line-misc-info
                          
                          ;; line and column
                          (propertize "%02l" 'face 'font-lock-type-face) ","
                          (propertize "%02c" 'face 'font-lock-type-face)

                          "  "
                          ;; vc-mode
                          '(:eval (modeline-vc-branch))
                          ))

         (modeline-left-right (append modeline-left modeline-right))
         (modeline-fill (append modeline-left modeline-right))
         )

    (cond
     ((> 91 (window-total-width)) modeline-left)
     (t modeline-fill))))


(defun update-modeline-format ()
  "Update modeline function."
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
  (setq x-underline-at-descent-line t
        auto-revert-check-vc-info t))


;; set modeline style
(update-modeline-format)


(provide 'init-modeline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-modeline.el ends here

;; (setq-default mode-line-format
;;               '(:eval
;;                 (list
;;                  (propertize " " 'display
;;                              '(image :type xpm :data "/* XPM */
;;           static char * close_tab_xpm[] = {
;;           \"20 11 3 1\",
;;           \"       c None\",
;;           \".      c #000000\",
;;           \"+      c #FFFFFF\",
;;           \"     .....          \",
;;           \"    .......         \",
;;           \"   .........        \",
;;           \"  ...+...+...       \",
;;           \"  ....+.+....       \",
;;           \"  .....+.....       \",
;;           \"  ....+.+....       \",
;;           \"  ...+...+...       \",
;;           \"   .........        \",
;;           \"    .......         \",
;;           \"     .....          \"};"
;;                                      :mask (heuristic t) :margin 0))
;;                  )
;;                 )
;;               )

(defvar moody--cache nil)

(defcustom moody-mode-line-height
  (let ((font (face-font 'mode-line)))
    (if font
        (ceiling (* (if (< emacs-major-version 27) 2 1.5)
                    (aref (font-info font) 2)))
      30))
  "When using `moody', height of the mode line in pixels.
This should be an even number."
  :type 'integer
  :group 'mode-line)


(defun moody-slant (direction c1 c2 c3 &optional height)
  (unless height
    (setq height moody-mode-line-height))
  (unless (cl-evenp height)
    (cl-incf height))
  (let ((key (list direction c1 c2 c3 height)))
    (or (cdr (assoc key moody--cache))
        (let* ((width (/ height 2))
               (image
                (create-image
                 (format "/* XPM */ static char * image[] = {
 \"%s %s 3 1\",\n \"0 c %s\",\n \"1 c %s\",\n \"2 c %s\",%s\n};"
                         width height c1 c2 c3
                         (cl-loop
                          for i from 1 to height concat
                          (format " \"%s\",\n"
                                  (let* ((x (/ i 2))
                                         (a (make-string x ?0))
                                         (b (make-string 1 ?1))
                                         (c (make-string
                                             (max 0 (- width x)) ?2)))
                                    (if (eq direction 'down)
                                        (concat a b c)
                                      (concat c b a))))))
                 'xpm t :ascent 'center)))
          (push (cons key image) moody--cache)
          image))))
