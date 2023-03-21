;;; init-company --- Summary

;;; Commentary:
;; some configuration of company.

;;; Code:


(use-package company
  :functions
  (company-box-icons--elisp
   my-company-yasnippet-disable-inline
   my-lsp-fix-company-capf
   company-backend-with-yas
   company--capf-data)
  :commands (company-cancel company-grab-line)
  :defines company-dabbrev-char-regexp
  :bind
  (("M-/" . company-complete)
   ("s-/" . company-yasnippet)
   :map company-active-map
   ("C-p"       . company-select-previous)
   ("C-n"       . company-select-next)
   ("<tab>"     . company-complete-common-or-cycle)
   :map company-search-map
   ("C-p" . company-select-previous)
   ("C-n" . company-select-next))
  :hook
  (after-init . global-company-mode)
  :custom
  (company-tooltip-align-annotations t)
  (company-tooltip-limit             12)
  (company-idle-delay                0)
  (company-echo-delay                nil)
  (company-minimum-prefix-length     1)
  (company-icon-margin               3)
  (company-require-match             nil)
  (company-dabbrev-ignore-case       nil)
  (company-dabbrev-code-ignore-case  nil)
  (company-dabbrev-downcase          nil)
  (company-global-modes '(not erc-mode message-mode help-mode shell-mode gud-mode))
  (company-backends     '((company-capf :with company-yasnippet)
                          (company-dabbrev-code company-keywords company-files)
                          company-dabbrev))
  :config
  ;; (defun only-company-yasnippet ()
  ;;   "Hide the current completeions and show snippets."
  ;;   (interactive)
  ;;   (company-abort)
  ;;   (call-interactively 'company-yasnippet))

  (defun company-anywhere-after-finish (completion)
    (when (and (stringp completion)
               (looking-at "\\(?:\\sw\\|\\s_\\)+")
               (save-match-data
                 (string-match (regexp-quote (match-string 0)) completion)))
      (delete-region (match-beginning 0) (match-end 0))))
  (add-hook 'company-after-completion-hook 'company-anywhere-after-finish)

  (defun company-anywhere-grab-word (_)
    (buffer-substring (point) (save-excursion (skip-syntax-backward "w") (point))))
  (advice-add 'company-grab-word :around 'company-anywhere-grab-word)

  (defun company-anywhere-grab-symbol (_)
    (buffer-substring (point) (save-excursion (skip-syntax-backward "w_") (point))))
  (advice-add 'company-grab-symbol :around 'company-anywhere-grab-symbol)

  (defun company-anywhere-dabbrev-prefix (_)
    (company-grab-line (format "\\(?:^\\| \\)[^ ]*?\\(\\(?:%s\\)*\\)" company-dabbrev-char-regexp) 1))
  (advice-add 'company-dabbrev--prefix :around 'company-anywhere-dabbrev-prefix)

  (defun company-anywhere-capf (fn command &rest args)
    (if (eq command 'prefix)
        (let ((res (company--capf-data)))
          (when res
            (let ((length (plist-get (nthcdr 4 res) :company-prefix-length))
                  (prefix (buffer-substring-no-properties (nth 1 res) (point))))
              (cond
               (length (cons prefix length))
               (t prefix)))))
      (apply fn command args)))
  (advice-add 'company-capf :around 'company-anywhere-capf)

  (defun company-anywhere-preview-show-at-point (pos completion)
    (when (and (save-excursion
                 (goto-char pos)
                 (looking-at "\\(?:\\sw\\|\\s_\\)+"))
               (save-match-data
                 (string-match (regexp-quote (match-string 0)) completion)))
      (move-overlay company-preview-overlay (overlay-start company-preview-overlay) (match-end 0))
      (let ((after-string (overlay-get company-preview-overlay 'after-string)))
        (when after-string
          (overlay-put company-preview-overlay 'display after-string)
          (overlay-put company-preview-overlay 'after-string nil)))))
  (advice-add 'company-preview-show-at-point :after 'company-anywhere-preview-show-at-point)

  ;; `yasnippet' integration
  (with-eval-after-load 'yasnippet
    (defun my-company-yasnippet ()
      "Hide the current completeions and show snippets."
      (interactive)
      (company-cancel)
      (call-interactively 'company-yasnippet))

    (defun company-backend-with-yas (backend)
      "Add `yasnippet' to company backend."
      (if (and (listp backend) (member 'company-yasnippet backend))
          backend
        (append (if (consp backend) backend (list backend))
                '(:with company-yasnippet))))

    (defun my-company-enbale-yas (&rest _)
      "Enable `yasnippet' in `company'."
      (setq company-backends (mapcar #'company-backend-with-yas company-backends)))

    (defun my-lsp-fix-company-capf ()
      "Remove redundant `comapny-capf'."
      (setq company-backends
            (remove 'company-backends (remq 'company-capf company-backends))))
    (advice-add #'lsp-completion--enable :after #'my-lsp-fix-company-capf)

    (defun my-company-yasnippet-disable-inline (fn cmd &optional arg &rest _ignore)
      "Enable yasnippet but disable it inline."
      (if (eq cmd  'prefix)
          (when-let ((prefix (funcall fn 'prefix)))
            (unless (memq (char-before (- (point) (length prefix)))
                          '(?. ?< ?> ?\( ?\) ?\[ ?{ ?} ?\" ?' ?`))
              prefix))
        (progn
          (when (and (bound-and-true-p lsp-mode)
                     arg (not (get-text-property 0 'yas-annotation-patch arg)))
            (let* ((name (get-text-property 0 'yas-annotation arg))
                   (snip (format "%s (Snippet)" name))
                   (len (length arg)))
              (put-text-property 0 len 'yas-annotation snip arg)
              (put-text-property 0 len 'yas-annotation-patch t arg)))
          (funcall fn cmd arg))))
    (advice-add #'company-yasnippet :around #'my-company-yasnippet-disable-inline))

  ;; Better sorting and filtering
  (use-package company-prescient
    :hook
    (company-mode . company-prescient-mode))

  ;; Icons and quickhelp
  (use-package company-box
    :diminish
    :commands
    (company-box-icons--elisp company-box--render-buffer company-box--get-frame company-box--make-frame company-box--compute-frame-position company-box--move-selection company-box--update-frame-position company-box--update-scrollbar company-box--get-buffer company-box--maybe-move-number company-box--display my-company-box--display company-box-doc--make-buffer my-company-box-doc--make-buffer my-company-box-icons--elisp)
    :hook
    (company-mode . company-box-mode)
    :custom
    (company-box-backends-colors nil)
    (company-box-show-single-candidate t)
    (company-box-doc-delay 0.1)
    (company-box-doc-frame-parameters '((vertical-scroll-bars . nil)
                                        (horizontal-scroll-bars . nil)
                                        (internal-border-width . 1)
                                        (left-fringe . 8)
                                        (right-fringe . 8)))
    :config
    ;; Prettify icons
    (defun my-company-box-icons--elisp (candidate)
      (when (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil)))))
    (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

    ;; Display borders and optimize performance
    (defun my-company-box--display (string on-update)
      "Display the completions."
      (company-box--render-buffer string on-update)

      (let ((frame (company-box--get-frame))
            (border-color (face-foreground 'font-lock-comment-face nil t)))
        (unless frame
          (setq frame (company-box--make-frame))
          (company-box--set-frame frame))
        (company-box--compute-frame-position frame)
        (company-box--move-selection t)
        (company-box--update-frame-position frame)
        (unless (frame-visible-p frame)
          (make-frame-visible frame))
        (company-box--update-scrollbar frame t)
        (set-face-background 'internal-border border-color frame)
        (when (facep 'child-frame-border)
          (set-face-background 'child-frame-border border-color frame)))
      (with-current-buffer (company-box--get-buffer)
        (company-box--maybe-move-number (or company-box--last-start 1))))
    (advice-add #'company-box--display :override #'my-company-box--display)

    (defun my-company-box-doc--make-buffer (object)
      (let* ((buffer-list-update-hook nil)
             (inhibit-modification-hooks t)
             (string (cond ((stringp object) object)
                           ((bufferp object) (with-current-buffer object (buffer-string))))))
        (when (and string (length> (string-trim string) 0))
          (with-current-buffer (company-box--get-buffer "doc")
            (erase-buffer)
            (insert (propertize "\n" 'face '(:height 0.5)))
            (insert string)
            (insert (propertize "\n\n" 'face '(:height 0.5)))

            ;; Handle hr lines of markdown
            ;; @see `lsp-ui-doc--handle-hr-lines'
            (let (bolp next before after)
              (goto-char 1)
              (while (setq next (next-single-property-change (or next 1) 'markdown-hr))
                (when (get-text-property next 'markdown-hr)
                  (goto-char next)
                  (setq bolp (bolp)
                        before (char-before))
                  (delete-region (point) (save-excursion (forward-visible-line 1) (point)))
                  (setq after (char-after (1+ (point))))
                  (insert
                   (concat
                    (and bolp (not (equal before ?\n)) (propertize "\n" 'face '(:height 0.5)))
                    (propertize "\n" 'face '(:height 0.5))
                    (propertize " "
                                'display '(space :height (1))
                                'company-box-doc--replace-hr t
                                'face `(:background ,(face-foreground 'font-lock-comment-face nil t)))
                    (propertize " " 'display '(space :height (1)))
                    (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.5))))))))

            (setq mode-line-format nil
                  display-line-numbers nil
                  header-line-format nil
                  show-trailing-whitespace nil
                  cursor-in-non-selected-windows nil)
            (current-buffer)))))
    (advice-add #'company-box-doc--make-buffer :override #'my-company-box-doc--make-buffer)
    

    (declare-function all-the-icons-faicon   'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon  'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.85 :v-adjust -0.2))
            (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.05))
            (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
            (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
            (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
            (Field . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
            (Variable . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
            (Class . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Interface . ,(all-the-icons-material "share" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Module . ,(all-the-icons-material "view_module" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.85 :v-adjust -0.2))
            (Value . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Enum . ,(all-the-icons-material "storage" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.85 :v-adjust -0.2))
            (Snippet . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2))
            (Color . ,(all-the-icons-material "palette" :height 0.85 :v-adjust -0.2))
            (File . ,(all-the-icons-faicon "file-o" :height 0.85 :v-adjust -0.05))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.85 :v-adjust -0.2))
            (Folder . ,(all-the-icons-faicon "folder-open" :height 0.85 :v-adjust -0.05))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Constant . ,(all-the-icons-faicon "square-o" :height 0.85 :v-adjust -0.05))
            (Struct . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-material "control_point" :height 0.85 :v-adjust -0.2))
            (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.05))
            (Template . ,(all-the-icons-material "format_align_left" :height 0.85 :v-adjust -0.2)))
          company-box-icons-alist 'company-box-icons-all-the-icons)))


(provide 'init-company)

;;; init-company.el ends here
