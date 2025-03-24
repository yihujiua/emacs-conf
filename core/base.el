;; The start screen is not displayed.
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")

(setq initial-buffer-choice
      '(lambda ()
         (let ((buf (get-buffer-create "*scratch*"))
               (file "~/.emacs.d/scratch.el"))
           (with-current-buffer buf
             (insert-file-contents file)
             (local-set-key
              "\C-x\C-s"
              '(lambda ()
                 (interactive)
                 (let ((str (buffer-string)))
                   (with-current-buffer
                       (find-file "~/.emacs.d/scratch.el")
                     (erase-buffer)   (insert str)
                     (save-buffer)    (kill-buffer)
                     )))))
           buf)))

(defalias 'yes-or-no-p 'y-or-n-p)
(global-visual-line-mode)
(column-number-mode)
(global-so-long-mode)
(global-auto-revert-mode)
(electric-pair-mode)
(setq-default fill-column 100)
(cua-mode)

;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-readable-p custom-file)
    (load custom-file))

(let (;; temporarily increase `gc-cons-threshold' when loading to speed up startup.
      (gc-cons-threshold most-positive-fixnum)
      ;; Empty to avoid analyzing files when loading remote files.
      (file-name-handler-alist nil))

  ;; Emacs configuration file content is written below.
  ;;
  ;; Todo:

  ;; How to loading! Main Plugin.
  ;; Keys
  (require 'lazy-load)             ;; Shortcut keys

  ;; Inline/Core
  (require 'service)               ;; Server
  (require 'userinfo)              ;; User
  (require 'encoding)              ;; Coding
  (require 'files)                 ;; Files
  (require 'themes)                ;; Themes

  ;; Inline/Edit
  (require 'linenumber)            ;; Text Number

  ;; Plugins
  (require 'in-orderless)          ;;
  (require 'in-vertico)            ;;
  (require 'in-marginalia)         ;;
  (require 'in-consult)            ;;
  (require 'in-projectile)         ;; Projects
  (require 'in-magit)              ;; Git
  (require 'in-web-mode)           ;; HTML/CSS
  (require 'in-js2-mode)           ;; JavaScript
  (require 'in-markdown-mode)      ;; Markdown
  (require 'in-dirvish)            ;; Explorer
  (require 'in-visual-fill-column) ;; Visual Fill Column
  (require 'in-flycheck)           ;; Syntax checking 

  ;; done
)

(provide 'base)
