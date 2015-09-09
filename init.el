;;; add MELPA, install use-package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
;(require 'diminish)                ; if you use :diminish
;(require 'bind-key)                ; if you use any :bind variant
;;; load packages
(use-package better-defaults
  :ensure t)
(use-package zenburn-theme ; this is the theme I use
  :ensure t
  :init
  (setq custom-safe-themes t)
  :config
  (load-theme 'zenburn))
(use-package magit ; for git
  :ensure t
  :bind ("C-c g" . magit-status)
  :init
  (defadvice git-commit-commit (after delete-window activate)
    (delete-window))
  (defadvice git-commit-abort (after delete-window activate)
    (delete-window)))
(use-package smex
  :ensure t)
(use-package ido-ubiquitous
  :ensure t)
(use-package paredit
  :ensure t)
(use-package find-file-in-project
  :ensure t)
(use-package markdown-mode ; for markdown mode
  :ensure t)
(use-package ess-site ; for ESS (Emacs Speaks Statistics)
  :ensure ess
  :mode ("\\.R" . r-mode))
(use-package auctex ; for LaTeX documents
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :init
  (progn
    (add-hook 'LaTeX-mode-hook 'visual-line-mode)
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
    (setq TeX-auto-save t
          TeX-parse-self t
          reftex-plug-into-AUCTeX t
          TeX-PDF-mode t)
  (setq-default TeX-master nil)))
(use-package polymode ; to have more than one major mode
  :ensure t
  :config
  :mode
  ("\\.md" . poly-markdown-mode)   
  ("\\.Snw" . poly-noweb+r-mode)   
  ("\\.Rnw" . poly-noweb+r-mode)  
  ("\\.Rmd" . poly-markdown+r-mode))
(use-package auto-complete ; auto completion
  :ensure t)
(use-package reftex ; bibliography and reference management 
  :commands turn-on-reftex)
(use-package neotree ; file tree plugin
  :ensure t
  :config (neotree))
(use-package flycheck ; checks for style and syntax 
  :ensure t
  :config (add-hook 'after-init-hook #'global-flycheck-mode))

;;; misc settings 
(setq inhibit-startup-message t) ; disable startup
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; start maximized
(global-set-key (kbd "C-z") 'undo) ; set "C-z" to undo, rather than minimize emacs (which seems useless)

;;; init.el ends here 
