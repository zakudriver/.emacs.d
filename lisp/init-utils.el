;;; init-uitls --- Summary

;;; Commentary:
;; somme configuration of utils.

;;; Code:


(eval-when-compile
  (require 'cl-lib)
  (require 'init-const)
  (require 'init-funcs)
  (require 'init-custom))


(use-package magit
  :custom
  (magit-diff-refine-hunk t)
  :config
  (with-no-warnings
    (defun my-magit-kill-buffers (&rest _)
      "Restore window configuration and kill all Magit buffers."
      (interactive)
      (magit-restore-window-configuration)
      (let ((buffers (magit-mode-get-buffers)))
        (when (eq major-mode 'magit-status-mode)
          (mapc (lambda (buf)
                  (with-current-buffer buf
                    (if (and magit-this-process
                             (eq (process-status magit-this-process) 'run))
                        (bury-buffer buf)
                      (kill-buffer buf))))
                buffers))))
    (setq magit-bury-buffer-function #'my-magit-kill-buffers)))


;; Access Git forges from Magit
(use-package forge
  :demand t
  :defines
  (forge-database-connector forge-topic-list-columns)
  :custom-face
  (forge-topic-label ((t (:inherit variable-pitch :height 0.9 :width condensed :weight regular :underline nil))))
  :custom
  (forge-database-connector (if (and (require 'emacsql-sqlite-builtin nil t)
                                     (functionp 'emacsql-sqlite-builtin)
                                     (functionp 'sqlite-open))
                                'sqlite-builtin
                              'sqlite))
  (forge-topic-list-columns '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
                              ("Title" 60 t nil title  nil)
                              ("State" 6 t nil state nil)
                              ("Updated" 10 t nil updated nil))))


(use-package ediff
  :ensure nil
  :custom
  (ediff-window-setup-function       'ediff-setup-windows-plain)
  (ediff-split-window-function       'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally)
  :hook
  ((ediff-prepare-buffer . outline-show-all) ;; show org ediffs unfolded
   (ediff-quit . winner-undo)))               ;; restore window layout when done


(use-package docker
  :bind
  ("C-c D". docker))


(use-package vterm
  :functions
  (my-bottom-window vterm-mode)
  :bind
  (("C-c v v" . vterm)
   :map vterm-mode-map
   ("M-p" . vterm-send-prior)
   ("M-n" . vterm-send-next)
   ("M-k" . vterm-send-up)
   ("M-j" . vterm-send-down)
   ("M-p" . vterm-yank)
   ("M-u" . vterm-undo))
  :config
  (advice-add #'vterm :override (lambda ()
                                  "Toggle vterm or create a new vterm."
                                  (interactive)

                                  (let ((buffer  (catch 'break
                                                   (dolist (i (buffer-list))
                                                     (when (string-match-p "vterm" (buffer-name i))
                                                       (throw 'break i)
                                                       )))))

                                    (if buffer
                                        (let ((win (get-buffer-window buffer)))
                                          (if (window-live-p win)
                                              (delete-window win)
                                            (let ((w (catch 'break
                                                       (dolist (i (window-list))
                                                         (let ((name (buffer-name (window-buffer i))))
                                                           (when (string-match-p "vterm" name)
                                                             ;; (delete-window name)
                                                             (throw 'break i)))))))
                                              (if w
                                                  (delete-window w)
                                                (my-bottom-window buffer)))))
                                      (setq buffer (generate-new-buffer "vterm"))
                                      (with-current-buffer (buffer-name buffer)
                                        (vterm-mode))
                                      (my-bottom-window buffer)))))

  (defvar my/vterm-buffer-list nil
    "Vterm buffer list.")

  (add-hook 'kill-buffer-hook (lambda ()
                                (if (derived-mode-p 'vterm-mode)
                                    (setq my/vterm-buffer-list
	                                        (delq (current-buffer) my/vterm-buffer-list)))))

  (add-hook 'vterm-mode-hook (lambda ()
                               (add-to-list 'my/vterm-buffer-list (current-buffer))))

  (defun my-vterm-switch (direction offset)
    (if my/vterm-buffer-list
        (let ((len (length my/vterm-buffer-list))
	            (index (cl-position (current-buffer) my/vterm-buffer-list)))
	        (if index
	            (let ((target-index (if (eq direction 'previous)
				                              (mod (+ index offset) len)
				                            (mod (- index offset) len))))
	              (switch-to-buffer (nth target-index my/vterm-buffer-list) nil t))
	          (switch-to-buffer (car my/vterm-buffer-list) nil t)))
      nil))

  (defun my-vterm-previous (&optional offset)
    "Go to the previous term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET."
    (interactive "P")
    (my-vterm-switch 'previous (or offset 1)))

  (defun my-vterm-next (&optional offset)
    "Go to the next term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET."
    (interactive "P")
    (my-vterm-switch 'next (or offset 1))))


;; process view
(use-package proced
  :ensure nil  
  :bind
  ("C-c p P" . 'proced)
  :custom
  (proced-auto-update-flag     t)
  (proced-auto-update-interval 3)
  (proced-format               'verbose))


;; Youdao Dictionary
(use-package yd
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/yd.el"
  :bind
  (("C-c y y" . yd-search-at-point-posframe)
   ("C-c y Y" . yd-search-at-point)
   ("C-c y I" . yd-search-from-input)
   :map yd-mode-map
   ("?" . yd-hydra/body))
  :custom
  (yd-app-key    "78762df07eff3cf2")
  (yd-secret-key "buZDTTRkFgVRPCegCzLpX255Y1Ql17F3")
  ;; 中文分词
  (yd-use-chinese-word-segmentation t)
  :init
  (setq url-automatic-caching t))


(use-package go-translate
  :custom
  (gts-translate-list '(("en" "zh") ("zh" "en")))
  :bind
  ("C-c y g" . gts-do-translate))


;; Fast search tool `ripgrep'
(use-package rg
  :defines projectile-command-map
  :hook
  (rg-mode . rg-enable-default-bindings)
  :bind
  (("C-c R m" . rg-menu)
   :map rg-mode-map
   ("m" . rg-menu))
  :custom
  ;; (rg-keymap-prefix "\C-cR")
  (rg-group-result t)
  (rg-show-columns t)
  :config
  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)

  (with-eval-after-load 'projectile
    (bind-key "s R" #'rg-project projectile-command-map)))


(use-package deadgrep
  :bind
  ("C-c R r" . deadgrep))


(use-package achive
  :load-path "~/.emacs.d/site-lisp/achive"
  :bind
  ("C-c a a" . achive)
  :custom
  (achive-stock-list '("sh600036" "sh601012" "sz000625" "sz002050" "sz002013" "sh600176" "sh603993" "sh601388" "sz002557" "sh600989" "sh600887" "sz002097" "sz000731" "sh601015" "sh601985" "sz000630" "sh600875" "sz002312" "sz000876" "sz000422" "sz00097")))


(use-package wttrin
  :load-path "~/.emacs.d/site-lisp/emacs-wttrin"
  :bind
  ("C-c w W" . wttrin)
  :custom
  (wttrin-cities           '("Guanghan"))
  (wttrin-forecast-days    2)
  (wttrin-language         "zh-cn")
  (wttrin-units-wind-speed t))


(use-package screenshot
  :load-path "~/.emacs.d/site-lisp/screenshot"
  :bind
  ("C-c s" . screenshot))


(use-package focus)


(use-package list-environment)


(use-package grep
  :ensure nil  
  :autoload grep-apply-setting
  :config
  (cond
   ((executable-find "ugrep")
    (grep-apply-setting
     'grep-command "ugrep --color=auto -0In -e ")
    (grep-apply-setting
     'grep-template "ugrep --color=auto -0In -e <R> <D>")
    (grep-apply-setting
     'grep-find-command '("ugrep --color=auto -0Inr -e ''" . 30))
    (grep-apply-setting
     'grep-find-template "ugrep <C> -0Inr -e <R> <D>"))
   ((executable-find "rg")
    (grep-apply-setting
     'grep-command "rg --color=auto --null -nH --no-heading -e ")
    (grep-apply-setting
     'grep-template "rg --color=auto --null --no-heading -g '!*/' -e <R> <D>")
    (grep-apply-setting
     'grep-find-command '("rg --color=auto --null -nH --no-heading -e ''" . 38))
    (grep-apply-setting
     'grep-find-template "rg --color=auto --null -nH --no-heading -e <R> <D>"))))


(provide 'init-utils)

;;; init-utils.el ends here
