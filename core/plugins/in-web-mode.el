;;
;;
(require 'web-mode)

(setq web-mode-markup-indent-offset 2) ;; HTML
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)
(setq web-mode-css-indent-offset 2)    ;; CSS
(setq web-mode-code-indent-offset 2)   ;; CODE


(add-to-list 'auto-mode-alist
	     '("\\.html?\\'" . web-mode)
	     '("\\.css?\\'" . web-mode)
	     )

(provide 'in-web-mode)
