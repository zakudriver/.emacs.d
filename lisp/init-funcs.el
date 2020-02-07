;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;; Save a file as utf-8
(defun save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

(defun kumo-kill-this-buffer (&optional arg)
  "Kill the current buffer.
   If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))

(defun kumo-kill-other-buffers (&optional arg)
  "Kill all other buffers.
   If the universal prefix argument is used then will the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))

(defun kumo-kill-all-buffers (&optional arg)
  "Kill all other buffers.
   If the universal prefix argument is used then will the windows too."
  (interactive "P")
  (mapc 'kill-buffer (buffer-list))
  (when (equal '(4) arg) (delete-other-windows))
  (message "Buffers deleted!"))

(defun kumo-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))


(defvar temp-number 0 "Temp-buffer tag.")
(defun kumo-new-temp-buffer ()
  "New a temp buffer."
  (interactive)
  (progn
    (switch-to-buffer-other-window (concat "*temp-" (number-to-string temp-number) "*"))
    (setq temp-number (+ temp-number 1))
    (insert (concat "// Happy hacking, " user-login-name " - Emacs ♥ you!"))))


(defun kumo-rename-current-buffer-file ()
  "Rename current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!")
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully rename to '%s'"
                   name (file-name-nodirecotry new-name)))))))

(defun kumo-delete-current-buffer-file ()
  "Removes file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun kumo-cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun kumo-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (kumo-cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

(defun kumo-rotate-window ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))


(defun kumo-toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))


;; Font
(defun kumo-font-size-increase ()
  "Font size increase."
  (interactive)
  (let ((old-face-attribute (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ old-face-attribute 10))))

(defun kumo-font-size-decrease ()
  "Font size decrease."
  (interactive)
  (let ((old-face-attribute (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (- old-face-attribute 10))))


;; Wrap selected text by input symbol.
(defun kumo-wrap-with-input (beg end symbol)
  "Wrap selected text by input symbol."
  (interactive "r \nsPlease input symbol: ")
  (let ((beg-symbol symbol)
        (end-symbol symbol))
    (cl-loop for (i . j) in kumo/symbol-list
             do (cond
                 ((string= symbol j)
                  (setq beg-symbol i))
                 ((string= symbol i)
                  (setq end-symbol j))
                 ))
    (goto-char beg)
    (insert beg-symbol)
    (goto-char (1+ end))
    (insert end-symbol)))


(defun kumo-window-vertically-selected ()
  "Split window vertically and selected."
  (interactive)
  (split-window-vertically)
  (other-window 1))


(defun kumo-window-horizontally-selected ()
  "Split window horizontally and selected."
  (interactive)
  (split-window-horizontally)
  (other-window 1))


(defun kumo-flycheck-list-errors-toggle ()
  "If flycheck errors list is living, to delete flycheck-errors window."
  (interactive)
  (let ((w (get-buffer-window kumo/flycheck-errors-buffer-name)))
    (if w
        (delete-window w)
      (flycheck-list-errors))))

(defun kumo-open-dashboard ()
  "Open the *dashboard* buffer and jump to the first widget."
  (interactive)

  (delete-other-windows)
  ;; Refresh dashboard buffer
  (if (get-buffer dashboard-buffer-name)
      (kill-buffer dashboard-buffer-name))
  (dashboard-insert-startupify-lists)
  (switch-to-buffer dashboard-buffer-name)

  ;; Jump to the first section
  (goto-line kumo/dashboard-position))



(provide 'init-funcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-funcs.el ends here
