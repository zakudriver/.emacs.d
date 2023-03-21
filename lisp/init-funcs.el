;;; init-funcs --- Summary

;;; Commentary:
;; some functions.

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (require 'init-custom))


;; Pakcage repository (ELPA)
(defun my-set-package-archives (archives &optional refresh async)
  "Set the package ARCHIVES (ELPA).

REFRESH is non-nil, will refresh archive contents.
ASYNC specifies whether to perform the downloads in the background."
  (interactive
   (list
    (intern
     (ivy-read "Select package archives: "
               (mapcar #'car my/package-archives-alist)
               :preselect (symbol-name my/package-archives)))))
  
  ;; Refresh if need
  (and refresh (package-refresh-contents async))

  (message "Set package archives to `%s'" archives))


(defun my-test-package-archives (&optional no-chart)
  "Test connection speed of all package archives and display on chart.

Not displaying the chart if NO-CHART is non-nil.
Return the fastest package archive."
  (interactive)

  (let* ((durations (mapcar
                     (lambda (pair)
                       (let ((url (concat (cdr (nth 2 (cdr pair)))
                                          "archive-contents"))
                             (start (current-time)))
                         (message "Fetching %s..." url)
                         (ignore-errors
                           (url-copy-file url null-device t))
                         (float-time (time-subtract (current-time) start))))
                     my/package-archives-alist))
         (fastest (car (nth (cl-position (apply #'min durations) durations)
                            my/package-archives-alist))))

    ;; Display on chart
    (when (and (not no-chart)
               (require 'chart nil t)
               (require 'url nil t))
      (if (functionp 'chart-bar-quickie)
          (chart-bar-quickie
           'horizontal
           "Speed test for the ELPA mirrors"
           (mapcar (lambda (p) (symbol-name (car p))) my/package-archives-alist)
           "ELPA"
           (mapcar (lambda (d) (* 1e3 d)) durations) "ms")))

    (message "`%s' is the fastest package archive" fastest)

    ;; Return the fastest
    fastest))


(defun my-open-init-file()
  "Open init.el file."
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))


(defun my-dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))


(defun my-unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))


(defun my-save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))


(defun my-font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))


(defun my-chars-displayable-p (chars)
  "Check if list of CHARS is available."
  (cl-loop for it in chars
           if (not (char-displayable-p (string-to-char it)))
           return nil
           finally return t))


(defun my-save-some-buffers ()
  "Save some buffers without prompting."
  (interactive)
  (if (y-or-n-p (format "Really save buffers? "))
      (save-some-buffers t)
    (message "Canceled save.")))


(defun my-kill-this-buffer (&optional arg)
  "Kill the current buffer.
ARG: If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if arg
        (kill-buffer-and-window)
      (kill-buffer)))
  (message "Buffers deleted!"))


(defun my-kill-other-buffers (&optional arg)
  "Kill other buffers.
ARG: If the universal prefix argument is used then will the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (mapc 'kill-buffer
          (delq (current-buffer) (buffer-list)))
    (when arg (delete-other-windows))
    (message "Buffers deleted!")))


(defun my-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))


(defun my-rename-current-buffer-file ()
  "Rename current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully rename to '%s'"
                   name (file-name-nondirectory new-name)))))))


(defun my-delete-current-buffer-file ()
  "Delete file connected to current buffer and kill buffer."
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


(defun my-cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a `before-save-hook',
and that might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))


(defun my-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (my-cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))


(defun my-toggle-window-split ()
  "Toggle windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((current-buffer (window-buffer))
             (next-buffer (window-buffer (next-window)))
             (current-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (current-win-2nd (not (and (<= (car current-win-edges)
                                            (car next-win-edges))
                                        (<= (cadr current-win-edges)
                                            (cadr next-win-edges)))))
             (splitter
              (if (= (car current-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if current-win-2nd (other-window 1))
          (set-window-buffer (selected-window) current-buffer)
          (set-window-buffer (next-window) next-buffer)
          (select-window first-win)
          (if current-win-2nd (other-window 1))))))


;; Wrap selected text by input symbol.
(defun my-wrap-with-input (beg end symbol)
  "Wrap selected text by input symbol.
BEG is begin point.
END is end point.
SYMBOL is input string."
  (interactive "*r\ncPlease input symbol: ")
  (setq symbol (char-to-string symbol))
  (let ((map-list '(("''" . "'")
                    ("\"\"" . "\"")
                    ("``" . "`"))))
    (cl-loop for (i . j) in map-list
             do (when (string= symbol i)
                  (setq symbol j))))

  (let ((beg-symbol symbol)
        (end-symbol symbol))
    (cl-loop for (i . j) in my/symbol-list
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


(defun my-window-vertically-selected ()
  "Split window vertically and selected."
  (interactive)
  (split-window-vertically)
  (other-window 1))


(defun my-window-horizontally-selected ()
  "Split window horizontally and selected."
  (interactive)
  (split-window-horizontally)
  (other-window 1))


(defun my-flycheck-list-errors-select-window ()
  "Select window for flycheck-errors-list."
  (interactive)
  (select-window (get-buffer-window my/flycheck-errors-buffer-name)))


(defun my-vterm-select-window ()
  "Select window for vterm."
  (interactive)
  (catch 'break
    (dolist (i (window-list))
      (let ((name (buffer-name (window-buffer i))))
        (when (string-match-p "vterm" name)
          (select-window (get-buffer-window name))
          (throw 'break nil))))))


(defun my-new-vterm ()
  "New a vterm."
  (interactive)
  (if (fboundp 'vterm-mode)
      (let ((buffer (generate-new-buffer "vterm")))
        (with-current-buffer (buffer-name buffer)
          (vterm-mode))
        (my-bottom-window buffer))
    (vterm)))


(defun my-bottom-window (buffer)
  "Open a bottom window.
BUFFER is the symbol."
  (display-buffer-in-side-window
   buffer
   '((side . bottom)
     (dedicated . t)
     (reusable-frames . visible)
     (window-height . 0.3))))


(defun my-current-buffer-bottom-window ()
  "Current buffer display on then bottom window."
  (interactive)
  (delete-window)
  (my-bottom-window (window-buffer)))


(defun my-indent-whole-buffer ()
  "Mark whole buffer."
  (interactive)
  (let ((point (point)))
    (push-mark)
    (push-mark (point-max) nil t)
    (goto-char (minibuffer-prompt-end))
    (indent-for-tab-command)
    (goto-char point)))


(defun my-home-path-resolve (&rest path)
  "Return $HOME + path.
PATH is a string list."
  (apply 'concat (getenv "HOME") path))


(defvar my/divisor-cache 14
  "Divisor cache.")
(defun my-number-division (beg end &optional num)
  "Selected number division.
BEG is begin point.
END is end point.
NUM is a number."
  (interactive "r\nsPlease input number: ")
  (setq num (string-to-number num))
  (setq my/divisor-cache (if (= num 0) my/divisor-cache num))
  (if (use-region-p)
      (let ((regionp (string-to-number (buffer-substring beg end))))
        (delete-region beg end)
        (insert
         (number-to-string (/ (float regionp) (float my/divisor-cache)))))))


(defun my-timestamp ()
  "Timestamp.
eg: 2020-02-22T22:22:22."
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%S")))


(defun my-select-minibuffer-window ()
  "Select minibuffer window."
  (interactive)
  (select-window (active-minibuffer-window)))


(defun my-winum-delete-window-factory (num)
  "Winum delete window function macro factory.
NUM is the window number."
  `(defun ,(intern (concat "winum-delete-window-" (number-to-string num))) ()
     (interactive)
     (,(intern-soft (concat "winum-select-window-" (number-to-string num))) t)))


(defun my-easy-hugo-github-deploy ()
  "Easy-Hugo deploy github page."
  (interactive)
  (let* ((output-buffer (get-buffer-create my/easy-hugo-github-deploy-buffer-name))
         (command-window (async-shell-command (expand-file-name (concat my/easy-hugo-basedir my/easy-hugo-github-deploy-script)) output-buffer nil)))
    (select-window command-window)))


(defun my-put-file-name-on-clipboard (&optional arg)
  "Put the current file name on the clipboard.
ARG: If the universal prefix argument is used then put file full name."
  (interactive "P")
  (let ((filename (if arg (buffer-file-name)
                    (file-name-nondirectory (buffer-file-name)))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))


(defun my-open-current-file-in-finder ()
  "Open current file in mac finder."
  (interactive)
  (when sys/macp
    (shell-command (concat "open -R " buffer-file-name))))


(defun my-org-inline-css-hook (exporter)
  "Insert custom inline css when org export html.
EXPORTER: export way."
  (when (eq exporter 'html)
    (let ((path (concat user-emacs-directory my/org-mode-export-html-css)))
      (if (file-exists-p path)
          (progn
            (setq-local org-html-head-include-default-style nil)
            (setq-local org-html-head (concat
                                       "<style type=\"text/css\">\n"
                                       "<!--/*--><![CDATA[/*><!--*/\n"
                                       (with-temp-buffer
                                         (insert-file-contents path)
                                         (buffer-string))
                                       "/*]]>*/-->\n"
                                       "</style>\n")))))))

;; === background opaciyt ===
(defun my-adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))


(defun my-adjust-opacity-down ()
  "Adjust the background opacity to down."
  (interactive)
  (my-adjust-opacity nil -2))


(defun my-adjust-opacity-up ()
  "Adjust the background opacity to up."
  (interactive)
  (my-adjust-opacity nil 2))


(defun my-adjust-opacity-max ()
  "Adjust the background opacity to max."
  (interactive)
  (modify-frame-parameters nil `((alpha . 100))))
;; === background opaciyt ===


(defun my-kill-whole-line ()
  "Kill region or whole line."
  (interactive)
  (if mark-active
      (delete-region (region-beginning)
                     (region-end))
    (delete-region (line-beginning-position)
                   (line-end-position))
    (delete-char 1)))


(defun my-newline-above-current ()
  "Add a line above current line like vim O."
  (interactive)
  (progn (beginning-of-line)
         (newline)
         (forward-line -1)))


(defun my-newline-next-current ()
  "Add a line next current line like vim o."
  (interactive)
  (progn (end-of-line)
         (newline-and-indent)))


(defun my-delete-word-at-point (&optional arg)
  "Delete word at point, like vim diw.
ARG: when not nil delete symbol( concat by '_') at point"
  (interactive "P")
  (let* (
         (type (if arg 'symbol 'word))
         (point (bounds-of-thing-at-point type)))
    (delete-region (car point) (cdr point))))


(defun my-delete-word (arg)
  "Kill characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))


(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))


(defun my-kill-line ()
  "Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
  (interactive)
  (let ((orig-point (point)))
    (move-end-of-line 1)
    (if (= orig-point (point))
        (delete-char 1)
      (delete-region
       orig-point
       (point)))))


(defun my-save-word-at-point (&optional arg)
  "Delete word at point, like vim diw.
ARG: when not nil delete symbol( concat by '_') at point"
  (interactive "P")
  (let* ((type (if arg 'symbol 'word))
         (point (bounds-of-thing-at-point type)))
    (kill-ring-save (car point) (cdr point))))


(defun my-find-left-bound ()
  "Find the left bound of an expr."
  (save-excursion (save-match-data
                    (let ((char (char-before))
                          (in-style-attr (looking-back "style=[\"'][^\"']*" nil))
                          (syn-tab (make-syntax-table)))
                      (modify-syntax-entry ?\\ "\\")
                      (while char
                        (cond ((and in-style-attr (member char '(?\" ?\')))
                               (setq char nil))
                              ((member char '(?\} ?\] ?\)))
                               (with-syntax-table syn-tab
                                 (backward-sexp) (setq char (char-before))))
                              ((eq char ?\>)
                               (if (looking-back "<[^>]+>" (line-beginning-position))
                                   (setq char nil)
                                 (progn (backward-char) (setq char (char-before)))))
                              ((not (string-match-p "[[:space:]\n;]" (string char)))
                               (backward-char) (setq char (char-before)))
                              (t
                               (setq char nil))))
                      (point)))))


(defun my-jsx-expand ()
  "JSX component expand.
E.g: <Button />"
  (interactive)
  (let* ((end (point))
         (start (my-find-left-bound))
         (line (buffer-substring-no-properties start end)))
    (delete-region start end)
    (insert (format "<%s />" line))
    (backward-char 3)))


(defun my-disable-window-dedicated ()
  "Disable current window is dedicated."
  (interactive)
  (set-window-dedicated-p (frame-selected-window) nil))


(defvar my/pre-window-configuration nil
  "Window configuration to use.")

(defun my-save-window-configuration ()
  "Save window configuration."
  (interactive)
  (setq my/pre-window-configuration (current-window-configuration)))


(defun my-restore-window-configuration ()
  "Restore window configuration."
  (interactive)
  (if (window-configuration-p my/pre-window-configuration)
      (set-window-configuration my/pre-window-configuration)))


(defun my-goto-matching-bracket ()
  "Move cursor to the matching bracket.
If cursor is not on a bracket, call `backward-up-list'.
The list of brackets to jump to is defined by `my-left-brackets'
and `my-right-brackets'."
  (interactive)
  (if (nth 3 (syntax-ppss))
      (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
    (cond
     ((eq (char-after) ?\") (forward-sexp))
     ((eq (char-before) ?\") (backward-sexp ))
     ((looking-at (regexp-opt my/left-brackets))
      (forward-sexp))
     ((looking-back (regexp-opt my/right-brackets) (max (- (point) 1) 1))
      (backward-sexp))
     (t (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)))))


(defun my-insert-eslint-disable-line ()
  "Insert eslint-disable-next-line."
  (interactive)
  (progn (beginning-of-line)
         (newline)
         (forward-line -1)
         (insert "// eslint-disable-next-line")
         (indent-for-tab-command)))


(defun my-insert-time-string ()
  "Select the formatted time to insert."
  (interactive)
  (let* ((fmt-list '("%Y-%m-%d" "%Y-%m-%d %a" "%Y-%m-%d %H:%M:%S" "%Y-%m-%d %H:%M:%S %a" "%Y/%m/%d" "%Y/%m/%d %a" "%Y/%m/%d %H:%M:%S" "%Y/%m/%d %H:%M:%S %a"))
         (str-list (mapcar 'format-time-string fmt-list)))
    
    (insert (completing-read "Please select the formatted time to insert: "
                             str-list
                             nil
                             t
                             nil
                             nil))))



(provide 'init-funcs)

;;; init-funcs.el ends here
