;;; init-uitls --- Summary

;;; Commentary:
;; somme configuration of utils.

;;; Code:


(eval-when-compile
  (require 'cl-lib)
  (require 'init-const)
  (require 'init-funcs)
  (require 'init-custom))


(use-package magit)


(use-package docker
  :bind
  ("C-c D". docker))


(use-package vterm
  :functions
  (kumo-bottom-window vterm-mode)
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
                                                (kumo-bottom-window buffer)))))
                                      (setq buffer (generate-new-buffer "vterm"))
                                      (with-current-buffer (buffer-name buffer)
                                        (vterm-mode))
                                      (kumo-bottom-window buffer)))))

  (defvar kumo/vterm-buffer-list nil
    "Vterm buffer list.")

  (add-hook 'kill-buffer-hook (lambda ()
                                (if (derived-mode-p 'vterm-mode)
                                    (setq kumo/vterm-buffer-list
	                                        (delq (current-buffer) kumo/vterm-buffer-list)))))

  (add-hook 'vterm-mode-hook (lambda ()
                               (add-to-list 'kumo/vterm-buffer-list (current-buffer))))

  (defun kumo-vterm-switch (direction offset)
    (if kumo/vterm-buffer-list
        (let ((len (length kumo/vterm-buffer-list))
	            (index (cl-position (current-buffer) kumo/vterm-buffer-list)))
	        (if index
	            (let ((target-index (if (eq direction 'previous)
				                              (mod (+ index offset) len)
				                            (mod (- index offset) len))))
	              (switch-to-buffer (nth target-index kumo/vterm-buffer-list) nil t))
	          (switch-to-buffer (car kumo/vterm-buffer-list) nil t)))
      nil))

  (defun kumo-vterm-previous (&optional offset)
    "Go to the previous term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET."
    (interactive "P")
    (kumo-vterm-switch 'previous (or offset 1)))

  (defun kumo-vterm-next (&optional offset)
    "Go to the next term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET."
    (interactive "P")
    (kumo-vterm-switch 'next (or offset 1))))


;; process view
(use-package proced
  :bind
  ("C-c p P" . 'proced)
  :custom
  (proced-auto-update-flag     t)
  (proced-auto-update-interval 3)
  (proced-format               'verbose))


;; Youdao Dictionary
(use-package yd
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
  (yd-use-chinese-word-segmentation t))


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
    (defalias 'projectile-ripgrep #'rg-project)
    (bind-key "s R" #'rg-project projectile-command-map))

  (with-eval-after-load 'counsel
    (bind-keys
     :map rg-global-map
     ("R" . counsel-rg)
     ("F" . counsel-fzf))))


(use-package deadgrep
  :bind
  ("C-c R r" . deadgrep))


(use-package achive
  :load-path "~/.emacs.d/site-lisp/achive"
  :bind
  ("C-c a a" . achive)
  ("C-c a g" . achive-update)
  :custom
  (achive-language   'zh)
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


(provide 'init-utils)

;;; init-utils.el ends here
