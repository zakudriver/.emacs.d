;;; Code:


;; Fullscreen
(if (featurep 'cocoa)
    (progn
      ;; 在Mac平台, Emacs不能进入Mac原生的全屏模式,否则会导致 `make-frame' 创建时也集成原生全屏属性后造成白屏和左右滑动现象.
      ;; 所以先设置 `ns-use-native-fullscreen' 和 `ns-use-fullscreen-animation' 禁止Emacs使用Mac原生的全屏模式.
      ;; 而是采用传统的全屏模式, 传统的全屏模式, 只会在当前工作区全屏,而不是切换到Mac那种单独的全屏工作区,
      ;; 这样执行 `make-frame' 先关代码或插件时,就不会因为Mac单独工作区左右滑动产生的bug.
      ;;
      ;; Mac平台下,不能直接使用 `set-frame-parameter' 和 `fullboth' 来设置全屏,
      ;; 那样也会导致Mac窗口管理器直接把Emacs窗口扔到单独的工作区, 从而对 `make-frame' 产生同样的Bug.
      ;; 所以, 启动的时候通过 `set-frame-parameter' 和 `maximized' 先设置Emacs为最大化窗口状态, 启动5秒以后再设置成全屏状态,
      ;; Mac就不会移动Emacs窗口到单独的工作区, 最终解决Mac平台下原生全屏窗口导致 `make-frame' 左右滑动闪烁的问题.
      (setq ns-use-native-fullscreen nil
            ns-use-fullscreen-animation nil)
      ;; 默认先最大化。
      (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
      (when (version< emacs-version "27")
        (run-at-time "2sec" nil
                     (lambda ()
                       (toggle-frame-fullscreen)
                       ))))
  ;; 非Mac平台直接最大化
  (toggle-frame-maximized))


;; Quickly switch windows
(use-package winum
  :hook
  (after-init . winum-mode)
  :bind (
         ("M-0" . 'winum-select-window-0)
         ("M-1" . 'winum-select-window-1)
         ("M-2" . 'winum-select-window-2)
         ("M-3" . 'winum-select-window-3)
         ("M-4" . 'winum-select-window-4)
         ("M-5" . 'winum-select-window-5)
         ("M-6" . 'winum-select-window-6)
         ("M-7" . 'winum-select-window-7)
         ("M-8" . 'winum-select-window-8)
         ("M-9" . 'winum-select-window-9))
  :custom
  (winum-auto-setup-mode-line nil))


(use-package hydra
  :init
  (defhydra hydra-frame-window (:color red :hint nil)
    "
 ^Window^             Frame^              ^^Window Size^^^      ^Text Zoom^               
 _0_: delete          _2_: delete              ^ ^ _k_ ^ ^            _=_                   
 _1_: delete others   _3_: delete others       _h_ ^+^ _l_            ^+^             
 _t_oggle             _4_: new                 ^ ^ _j_ ^ ^            _-_            
 _s_wap               _F_ullscreen            ^_b_alance^^^^          ^ ^        
"
    ("0" delete-window)
    ("1" delete-other-windows)
    ("2" delete-frame :exit t)
    ("3" delete-other-frames :exit t)
    ("4" make-frame  :exit t)
    ("b" balance-windows)
    ("s" kumo-toggle-window-split)
    ("F" toggle-frame-fullscreen)
    ("t" kumo-rotate-window)
    ("=" kumo-font-size-increase)
    ("-" kumo-font-size-decrease)
    ("h" shrink-window-horizontally)
    ("k" shrink-window)
    ("j" enlarge-window)
    ("l" enlarge-window-horizontally)
    ("q" nil "quit")))


(provide 'init-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-window.el ends here
