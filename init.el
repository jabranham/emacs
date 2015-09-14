;;; init.el --- my emacs configuration

;;; Commentary:
; This is how I set up my emacs
; See https://github.com/jabranham/emacs for a readme on how to get set up


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
;(require 'diminish) ; if you use :diminish
(require 'bind-key) ; if you use any :bind variant
(require 'cl-lib) ; require common lisp expressions
(require 'cl) ; require common lisp

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
  :bind ("C-c g" . magit-status)
  :config
  (setq magit-push-always-verify nil))

(use-package smex
  :ensure t)

(use-package ido-ubiquitous
  :ensure t)

(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-mode 1) ; turn on ido mode
  (ido-vertical-mode 1) ; turn on ido vertical mode
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)) ; make up and down keys work

(use-package smartparens-config ; makes parens easier to keep track of
  :ensure smartparens
  :config
  (smartparens-global-mode 1)
  (show-smartparens-global-mode +1))

(use-package find-file-in-project
  :ensure t)

(use-package markdown-mode ; for markdown mode
  :ensure t)

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
  :mode
  ("\\.Snw" . poly-noweb+r-mode)
  ("\\.Rnw" . poly-noweb+r-mode)
  ("\\.Rmd" . poly-markdown+r-mode))

(use-package company ; auto completion
  :ensure t)

(use-package company-statistics
  :ensure t
  :config
  (company-statistics-mode)
  (define-key company-active-map (kbd "<tab>")
    (lambda () (interactive) (company-complete-common-or-cycle 1)))
  (global-company-mode t))

(use-package reftex ; bibliography and reference management
  :commands turn-on-reftex)

(use-package flycheck ; checks for style and syntax
  :ensure t
  :config (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package smooth-scrolling ; stops emacs nonsense default scrolling
  :ensure t
  :config
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; one line at a time
  (setq mouse-wheel-progressive-speed nil) ; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ; scroll window under mouse
  (setq scroll-step 1)) ; keyboard scroll one line at a time

(use-package flyspell ; spell checking on the fly
  :ensure t
  :init
  (setq flyspell-sort-corrections nil)
  (autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
  :config
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'markdown-mode-hook 'flyspell-mode))

(use-package latex-pretty-symbols ; makes latex math look a bit better in the editor
  :ensure t)

(use-package whitespace-cleanup-mode ; cleans up whitespace from specified modes
  :ensure t
  :config
  (add-hook 'haskell-mode 'whitespace-cleanup-mode)
  (add-hook 'emacs-lisp-mode 'whitespace-cleanup-mode)
  (add-hook 'lisp-mode 'whitespace-cleanup-mode)
  (add-hook 'scheme-mode 'whitespace-cleanup-mode)
  (add-hook 'ess-mode 'whitespace-cleanup-mode)
  (add-hook 'erlang-mode 'whitespace-cleanup-mode)
  (add-hook 'clojure-mode 'whitespace-cleanup-mode)
  (add-hook 'ruby-mode 'whitespace-cleanup-mode)
  (add-hook 'stan-mode 'whitespace-cleanup-mode))


;; misc settings
(setq inhibit-startup-message t ; disable start screen
      global-font-lock-mode t ; font lock (syntax highlighting) everywhere
      font-lock-maximum-decoration t) ; lots of color
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; start maximized
(global-set-key (kbd "C-z") 'undo) ; set "C-z" to undo, rather than minimize emacs (which seems useless)
(define-key global-map (kbd "C-+") 'text-scale-increase) ; C-+ increases font size
(define-key global-map (kbd "C--") 'text-scale-decrease) ; C-- decreases font size


;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ess-R-font-lock-keywords
   (quote
    ((ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:constants . t)
     (ess-fl-keyword:fun-calls . t)
     (ess-fl-keyword:numbers . t)
     (ess-fl-keyword:operators . t)
     (ess-fl-keyword:delimiters . t)
     (ess-fl-keyword:= . t)
     (ess-R-fl-keyword:F&T . t)
     (ess-R-fl-keyword:%op% . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
