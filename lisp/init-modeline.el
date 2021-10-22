;;; init-modeline --- Summary

;;; Commentary:
;; some configuration of modeline.

;;; Code:

(require 'cl-lib)


(eval-when-compile
  (require 'init-const)
  (require 'init-custom))


(defconst modeline-space
  (if sys/macp "    " " ")
  "Mac or Linux space size.")


(defvar modeline--cache nil)


(defvar modeline--active-window (selected-window))


(defun modeline-slant (direction c1 c2 c3)
  "Modeline split line.
DIRECTION is 'left or 'right.
C1 is right color.
C2 is line color.
C3 is left color."
  (let ((key (list direction c1 c2 c3 kumo/modeline-height)))
    (or (cdr (assoc key modeline--cache))
        (let* ((width (/ kumo/modeline-height 2))
               (image
                (create-image
                 (format "/* XPM */ static char * image[] = {
 \"%s %s 3 1\",\n \"0 c %s\",\n \"1 c %s\",\n \"2 c %s\",%s\n};"
                         width kumo/modeline-height c1 c2 c3
                         (cl-loop
                          for i from 1 to kumo/modeline-height concat
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
          (push (cons key image) modeline--cache)
          image))))


(defun modeline-window-active-p ()
  "Return t if the selected window is the active window.
Or put differently, return t if the possibly only temporarily
selected window is still going to be selected when we return
to the command loop."
  (if (fboundp 'old-selected-window)
      (or (eq (selected-window)
              (old-selected-window))
          (and (not (zerop (minibuffer-depth)))
	             (eq (selected-window)
	                 (with-selected-window (minibuffer-window)
	                   (minibuffer-selected-window)))))
    (eq (selected-window) modeline--active-window)))


(defun modeline-wrap (string &optional width direction type line-position)
  "Modeline element wrapper.
STRING is the element.
WIDTH is white space.
DIRECTION is 'up or 'down.
TYPE is a bool, direction of dip.
LINE-POSITION is 'top or 'bottom or 'all."
  (unless direction
    (setq direction 'down))
  (let* ((base  (if (modeline-window-active-p) 'mode-line 'mode-line-inactive))
         (outer (face-attribute base :background))
         ;; (line  (face-attribute base :underline))
         (line (face-attribute base :background))
         (line (if (listp line) (plist-get line :color) line))
         (line (if (eq line 'unspecified) outer line))
         (inner (face-attribute 'default :background))
         (slant (if (eq direction 'down)
                    (list outer line inner)
                  (list inner line outer)))
         (face (list :overline nil
                     :underline nil
                     :background inner))
         (pad (max (- (or width 0) (length string)) 2)))

    ;; (when (eq line-position 'all)
    ;;   (setq face
    ;;         (list :overline line
    ;;               :underline line
    ;;               :background inner)))

    ;; (when (eq line-position 'top)
    ;;   (setq face
    ;;         (list :overline line
    ;;               :underline nil
    ;;               :background inner)))

    ;; (when (eq line-position 'bottom)
    ;;   (setq face
    ;;         (list :overline nil
    ;;               :underline line
    ;;               :background inner)))

    ;; (unless line-position
    ;;   (setq face
    ;;         (list :overline nil
    ;;               :underline nil
    ;;               :background inner)))

    (setq string
          (concat (make-string (ceiling pad 2) ?\s)
                  (substring string 0)
                  (make-string (floor pad 2) ?\s)))
    (add-face-text-property 0 (length string) face nil string)
    (list
     (propertize " " 'face face 'display
                 (apply 'modeline-slant
                        (if (eq direction 'down) 'down 'up)
                        slant))
     string
     (if type
         (propertize " " 'face face 'display
                     (apply 'modeline-slant
                            (if (eq direction 'down) 'down 'up)
                            (if (eq direction 'down)
                                (list inner line outer)
                              (list outer line inner))))
       (propertize " " 'face face 'display
                   (apply 'modeline-slant
                          (if (eq direction 'down) 'up 'down) slant)))
     )))


;; nyan-mode
(use-package nyan-mode
  :hook
  (after-init . nyan-mode)
  :custom
  (nyan-bar-length 24)
  (nyan-animate-nyancat nil)
  (nyan-wavy-trail nil)
  (nyan-animation-frame-interval 0.4))


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
(defun modeline-flycheck ()
  "Modeline flycheck."
  (when (and flycheck-last-status-change (eq flycheck-last-status-change 'finished))
    (modeline-wrap
     (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
            (no-errors (cdr (assq 'error error-counts)))
            (no-warnings (cdr (assq 'warning error-counts)))
            (face (cond (no-errors 'error)
                        (no-warnings 'warning)
                        (t 'success))))
       (propertize (format " † %s ‡ %s " (or no-warnings 0) (or no-errors 0))
                   'face `(,face (:weight bold))))
     ;; (pcase flycheck-last-status-change
     ;;   (`not-checked nil)
     ;;   (`no-checker (propertize " -" 'face 'warning))
     ;;   (`running (propertize " ✷" 'face 'success))
     ;;   (`errored (propertize " !" 'face 'error))
     ;;   (`finished
     ;;    (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
     ;;           (no-errors (cdr (assq 'error error-counts)))
     ;;           (no-warnings (cdr (assq 'warning error-counts)))
     ;;           (face (cond (no-errors 'error)
     ;;                       (no-warnings 'warning)
     ;;                       (t 'success))))
     ;;      (propertize (format " † %s ‡ %s " (or no-warnings 0) (or no-errors 0))
     ;;                  'face `(,face (:weight bold)))))
     ;;   (`interrupted " -")
     ;;   (`suspicious '(propertize " ?" 'face 'warning))
     ;;   )
     0 'up nil 'all)))


;; vc-mode
(defun modeline-vc-branch ()
  "Git branch."
  (when vc-mode
    (let* ((noback (replace-regexp-in-string (format "^ %s" (vc-backend buffer-file-name)) " " vc-mode))
           (face (cond ((string-match "^ -" noback) 'mode-line-vc)
                       ((string-match "^ [:@]" noback) 'mode-line-vc-edit)
                       ((string-match "^ [!\\?]" noback) 'mode-line-vc-modified))))
      (substring noback 2)))
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
        (setq M-files (concat M-files "\n" line)))

       (t
        (setq O (+ 1 O))
        (setq O-files (concat O-files "\n" line)))))
    ;; construct propertized string
    (format " %d / %d / %d " M U O)))


(defun modeline-renderer ()
  "Mode line renderer."
  (let* (
         (modeline-left (list
                         modeline-space

                         ;; winum
                         (propertize
                          (modeline-unicode-number (winum-get-number-string))
                          'face 'font-lock-preprocessor-face)

                         modeline-space

                         ;; is Modified
                         (propertize (modeline-modified-p) 'face 'font-lock-string-face)

                         modeline-space
                         
                         ;; the buffer name; the file name as a tool tip
                         (modeline-wrap
                          (propertize "%b " 'face '(font-lock-keyword-face (:weight bold))
                                      'help-echo (buffer-file-name))
                          6 'down nil 'all)

                         modeline-space
                         
                         ;; size
                         (propertize "%I" 'face 'font-lock-constant-face)

                         modeline-space

                         ;; evil state
                         '(:eval evil-mode-line-tag)

                         ;; major mode
                         (modeline-wrap
                          (propertize "%m" 'face '(font-lock-string-face (:weight bold)))
                          6 'down nil 'all)


                         '(:eval (nyan-create))

                         '(:eval (modeline-flycheck))

                         ;; git info
                         (propertize
                          (modeline-git-status) 'face '(font-lock-string-face (:weight bold)))
                         ))
         (modeline-right (list
                          (modeline-wrap (modeline-fill (if sys/macp 16 18)) 0 'up t 'all)

                          modeline-space

                          ;; line and column
                          (propertize "%02l" 'face 'font-lock-type-face) ","
                          (propertize "%02c" 'face 'font-lock-type-face)

                          modeline-space

                          '(:eval (modeline-vc-branch))
                          ))

         (modeline-fill (append modeline-left modeline-right))
         (width (string-width (format-mode-line modeline-fill)))
         )

    (cond
     ((> width (window-total-width)) modeline-left)
     (t modeline-fill))))


(defun update-modeline-format ()
  "Update modeline function."
  (setq-default mode-line-format '(:eval (modeline-renderer)))
  (set-face-attribute 'mode-line nil
                      :family "SF Pro Text"
                      :height 100
                      :overline nil
                      :underline nil
                      :background (face-attribute 'mode-line :background)
                      :box nil
                      )
  (set-face-attribute 'mode-line-inactive  nil
                      :family "SF Pro Text"
                      :height 90
                      :overline nil
                      :underline nil
                      :background (face-background 'default)
                      :box nil
                      ))


(setq x-underline-at-descent-line t
      auto-revert-check-vc-info t
      ns-use-srgb-colorspace nil)


;; set modeline style
(update-modeline-format)


(provide 'init-modeline)

;;; init-modeline.el ends here
