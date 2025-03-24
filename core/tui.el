;;
;;
;; 禁用图像元素
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-x-resources t)

;; Menu
(menu-bar-mode 0)

;; Mouse
(global-unset-key [mouse-movement])

;; 优化终端渲染性能
(setq bidi-daplay-reordering 'left-to-right
      bidi-paragraph-direction 'left-to-right
      redisplay-skip-fontification-on-input t)

;; 调整垃圾回收策略
(setq gc-cons-threshold (* 64 10124 1024))
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 16 1024 1024))))

;; 解决终端中的 Meta 键延迟
;;(setq meta-prefix-char '?\ M-x
;;      x-alt-keysym 'meta)

(provide 'tui)
