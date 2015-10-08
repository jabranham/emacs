;;; init.el --- my emacs configuration

;;; Commentary:
; This is how I set up my emacs
; See https://github.com/jabranham/emacs for a readme on how to get set up


;;; Code:
(setq gc-cons-threshold 20000000)
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
  :bind
  ("C-c g" . magit-status)
  ("C-x g" . magit-status)
  :config
  (setq magit-push-always-verify nil)
  ;; This code makes magit-status run alone in the frame,
  ;; and then restores the old window configuration when you quit out of magit.
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defun magit-quit-session ()
      "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))
    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

(use-package smex
  :ensure t
  :bind
  ("M-x" . smex)
  ("C-c C-c M-x" . execute-extended-command)
  ("M-X" . smex-major-mode-commands))

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
    (setq-default TeX-master nil))
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-mode t)
  (setq TeX-view-program-selection '((output-pdf "pdf-tools")))
  (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))) ; set up pdf-tools as pdf viewer

(pdf-tools-install) ; nice PDF viewer (needs separate installation)

(use-package auctex-latexmk ; enables latexmk
  :ensure t
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "LatexMk"))))

(use-package polymode ; to have more than one major mode
  :ensure t
  :mode
  ("\\.Snw" . poly-noweb+r-mode)
  ("\\.Rnw" . poly-noweb+r-mode)
  ("\\.Rmd" . poly-markdown+r-mode))

(use-package company ; auto completion
  :ensure t
  :config
  (use-package company-statistics
    :ensure t
    :config
    (company-statistics-mode)
    (define-key company-active-map (kbd "<tab>")
      (lambda () (interactive) (company-complete-common-or-cycle 1)))
    (global-company-mode t))
  (use-package company-auctex
    :ensure t
    :config
    (company-auctex-init))
  (use-package company-math
    :ensure t)
  (use-package company-quickhelp
    :ensure t)
)

(use-package reftex ; bibliography and reference management
  :commands turn-on-reftex
  :config
  (setq reftex-cite-format ; set up reftex to work with biblatex (not natbib) and pandoc
      '((?\C-m . "\\cite[]{%l}")
        (?t . "\\textcite{%l}")
        (?a . "\\autocite[]{%l}")
        (?p . "\\parencite{%l}")
        (?f . "\\footcite[][]{%l}")
        (?F . "\\fullcite[]{%l}")
        (?P . "[@%l]")
        (?T . "@%l [p. ]")
        (?x . "[]{%l}")
        (?X . "{%l}"))))

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
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (add-hook 'text-mode-hook 'turn-on-flyspell)
  (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
  (add-hook 'markdown-mode-hook 'turn-on-flyspell)
  (add-hook 'org-mode-hook 'turn-on-flyspell))

(use-package latex-pretty-symbols ; makes latex math look a bit better in the editor
  :ensure t)

(use-package whitespace-cleanup-mode ; cleans up whitespace from specified modes
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'whitespace-cleanup-mode)
  (add-hook 'emacs-lisp-mode-hook 'whitespace-cleanup-mode)
  (add-hook 'lisp-mode-hook 'whitespace-cleanup-mode)
  (add-hook 'scheme-mode-hook 'whitespace-cleanup-mode)
  (add-hook 'ess-mode-hook 'whitespace-cleanup-mode)
  (add-hook 'erlang-mode-hook 'whitespace-cleanup-mode)
  (add-hook 'clojure-mode-hook 'whitespace-cleanup-mode)
  (add-hook 'ruby-mode-hook 'whitespace-cleanup-mode)
  (add-hook 'stan-mode-hook 'whitespace-cleanup-mode))

(use-package ebib ; for .bib files
  :ensure t
  :config
  (setq ebib-preload-bib-files
        '("~/Dropbox/library.bib")))

(use-package dired+
  :ensure t)

(use-package rainbow-delimiters ; for nice coloring of parens
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'ess-mode-hook #'rainbow-delimiters-mode))

(use-package smart-mode-line
  :ensure t
  :config
  (sml/setup))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Set up ESS style
(add-to-list 'ess-style-alist
             '(my-style
               (ess-indent-level . 4)
               (ess-first-continued-statement-offset . 2)
               (ess-continued-statement-offset . 0)
               (ess-brace-offset . -4)
               (ess-expression-offset . 4)
               (ess-else-offset . 0)
               (ess-close-brace-offset . 0)
               (ess-brace-imaginary-offset . 0)
               (ess-continued-brace-offset . 0)
               (ess-arg-function-offset . 4)
           (ess-arg-function-offset-new-line . '(4))
               ))

(setq ess-default-style 'my-style)

;; misc settings
(setq inhibit-startup-message t ; disable start screen
      global-font-lock-mode t ; font lock (syntax highlighting) everywhere
      font-lock-maximum-decoration t) ; lots of color
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; start maximized
(global-set-key (kbd "C-z") 'undo) ; set "C-z" to undo, rather than minimize emacs (which seems useless)
(define-key global-map (kbd "C-+") 'text-scale-increase) ; C-+ increases font size
(define-key global-map (kbd "C--") 'text-scale-decrease) ; C-- decreases font size
(if window-system ; show menu if emacs is window, not if terminal
    (menu-bar-mode t)
    (menu-bar-mode -1)
    )
(set-default 'indent-tabs-mode nil) ; don't use tabs
(global-set-key (kbd "M-/") 'hippie-expand) ; use M-/ for hippie expand
;; resizing 'windows' (i.e., inside the frame)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(define-key global-map (kbd "<S-down-mouse-1>") 'mouse-save-then-kill) ; shift-selection with mouse

(fset 'yes-or-no-p 'y-or-n-p) ; type y or n instead of yes RET and no RET

;; kill line if no region active 
;; http://emacs-fu.blogspot.co.uk/2009/11/copying-lines-without-selecting-them.html
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

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
