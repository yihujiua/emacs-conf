;;
;; Line numbers are not displayed when large files are used.
(setq line-number-display-limit large-file-warning-threshold)
(setq line-number-display-limit-width 1000)

(dolist (hook (list
	       'emacs-lisp-mode-hook
	       ))
  (add-hook hook (lambda ()
		   (display-line-numbers-mode))))

(provide 'linenumber)
