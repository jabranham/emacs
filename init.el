;;; init.el --- my emacs condiguration

;;; Commentary:
; This is how I set up my emacs

;;; Code:
;; add MELPA, install use-package
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
(require 'bind-key)                ; if you use any :bind variant

;; load packages
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
  :bind ("C-c g" . magit-status))
(use-package smex
  :ensure t)
(use-package ido-ubiquitous
  :ensure t)
(use-package smartparens-config ; makes parens easier to keep track of
  :ensure smartparens
  :config
  (smartparens-global-mode 1)
  (show-smartparens-global-mode +1))
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
  :disabled t ; this makes magit misbehave - need to fix
  :ensure t
  :config (neotree))
(use-package flycheck ; checks for style and syntax
  :ensure t
  :config (add-hook 'after-init-hook #'global-flycheck-mode))
(use-package smooth-scrolling
  :ensure t
  :config
  (setq-default
   scroll-conservatively 0
   scroll-up-aggressively 0.01
   scroll-down-aggressively 0.01))
(use-package flyspell
  :ensure t
  :config
  (setq ispell-program-name "aspell"
        ispell-dictionary "english"
        ispell-dictionary-alist
        (let ((default '("[A-Za-z]" "[^A-Za-z]" "[']" nil
                         ("-B" "-d" "english")
                         nil iso-8859-1)))
          `((nil ,@default)
            ("english" ,@default))))
  (setq ispell-extra-args '("--sug-mode=ultra"))
  (setq ispell-personal-dictionary "~/.aspell.en.pws")
  (setq flyspell-issue-message-flag nil))

;;; misc settings
(setq inhibit-startup-message t) ; disable startup
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; start maximized
(global-set-key (kbd "C-z") 'undo) ; set "C-z" to undo, rather than minimize emacs (which seems useless)

;;; init.el ends here
