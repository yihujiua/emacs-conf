
(require 'markdown-mode)

;;(autoload 'markdown-mode "markdown-mode"
;;  "Major mode for editing Markdown files" t)

;;(add-to-list 'auto-mode-alist
;;             '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-e") #'markdown-do))

(setq markdown-command "cmark-gfm")


(provide 'in-markdown-mode)
