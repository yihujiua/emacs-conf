
(require 'js2-mode)

(add-to-list 'auto-mode-alist
	     '("\\.js[x]?\\'" js2-mode)
	     '("\\.js\\'" js2-mode))

(provide 'in-js2-mode)
