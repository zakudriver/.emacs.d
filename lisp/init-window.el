;;; init-window --- Summary

;;; Commentary:
;; some configuration of window.

;;; Code:


(eval-when-compile
  (require 'init-const)
  (require 'init-funcs))


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
      
      (set-frame-parameter (selected-frame) 'fullscreen 'maximized))
  ;; 非Mac平台直接最大化
  (toggle-frame-maximized))


;; Quickly switch windows
(use-package winum
  :hook
  (after-init . winum-mode)
  :bind (("M-0"     . winum-select-window-0)
         ("M-1"     . winum-select-window-1)
         ("M-2"     . winum-select-window-2)
         ("M-3"     . winum-select-window-3)
         ("M-4"     . winum-select-window-4)
         ("M-5"     . winum-select-window-5)
         ("M-6"     . winum-select-window-6)
         ("M-7"     . winum-select-window-7)
         ("M-8"     . winum-select-window-8)
         ("M-9"     . winum-select-window-9)
         ("C-x M-0" . winum-delete-window-0)
         ("C-x M-1" . winum-delete-window-1)
         ("C-x M-2" . winum-delete-window-2)
         ("C-x M-3" . winum-delete-window-3)
         ("C-x M-4" . winum-delete-window-4)
         ("C-x M-5" . winum-delete-window-5)
         ("C-x M-6" . winum-delete-window-6)
         ("C-x M-7" . winum-delete-window-7)
         ("C-x M-8" . winum-delete-window-8)
         ("C-x M-9" . winum-delete-window-9))
  :custom
  (winum-auto-setup-mode-line nil)
  :config
  (kumo-winum-delete-window-macro-factory))


(use-package hydra
  :init
  (defhydra hydra-frame-window (:color pink :hint nil)
    "
 ^Window^                              Frame^                       ^^Window Size^^^       ^Opacity^              
 _w1_: delete                          _f1_: delete                     ^ ^ _K_ ^ ^            _=_          
 _w2_: delete others                   _f2_: delete others              _H_ ^+^ _L_            ^+^ 
 _s_wap x-direction and y-direction    _f3_: new                        ^ ^ _J_ ^ ^            _-_  
 Flip _v_erticall                                                   _F_ullscreen        ^^^_m_ax
 Flop _h_orizontally                                                _M_aximized
 _R_otate 180 degrees                                               _b_alance
 Rotate 90  degrees _c_lockwise
 Rotate 90  degrees _a_nti-clockwise
"
    ("w1" delete-window)
    ("w2" delete-other-windows)
    ("f1" delete-frame :exit t)
    ("f2" delete-other-frames :exit t)
    ("f3" make-frame  :exit t)
    ("b"  balance-windows)
    ("s"  transpose-frame)
    ("F"  toggle-frame-fullscreen)
    ("M"  toggle-frame-maximized)
    ("H"  shrink-window-horizontally)
    ("K"  shrink-window)
    ("J"  enlarge-window)
    ("L"  enlarge-window-horizontally)
    ("v"  flip-frame)
    ("h"  flop-frame)
    ("R"  rotate-frame)
    ("c"  rotate-frame-clockwise)
    ("a"  rotate-frame-anticlockwise)
    ("-"  kumo-adjust-opacity-down)
    ("="  kumo-adjust-opacity-up)
    ("m"  kumo-adjust-opacity-max)
    ("q"  nil "quit"))
  :config
  (use-package transpose-frame))


(use-package zoom
  :hook
  (after-init . zoom-mode)
  :bind
  ("C-c w z" . zoom)
  :custom
  (zoom-size                 '(0.618 . 0.618))
  (zoom-ignored-buffer-names '(kumo/flycheck-errors-buffer-name))
  (zoom-ignored-major-modes  '(flycheck-error-list-mode undo-tree-visualizer-mode achive-visual-mode treemacs-mode vterm-mode))
  :config
  (advice-add 'balance-windows :around (lambda (func &optional window-or-frame)
                                         (unless (zoom--window-ignored-p)
                                           (funcall func window-or-frame)))))


(provide 'init-window)

;;; init-window.el ends here
