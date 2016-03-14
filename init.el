;;; init.el --- Where all the magic begins

; Minimum version is emacs 24
(let ((minver 24))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;;
;; This file loads Org-mode and then loads the rest of our Emacs initialization from Emacs lisp
;; embedded in literate Org-mode files.

;; Load up Org Mode and (now included) Org Babel for elisp embedded in Org Mode files
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))


(setq inhibit-startup-message t ; disable start screen
      global-font-lock-mode t ; font lock (syntax highlighting) everywhere
      font-lock-maximum-decoration t) ; lots of color
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ; start maximized
(setq gc-cons-threshold 20000000)

;; add MELPA, install use-package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(unless (package-installed-p 'org-plus-contrib)
  (package-refresh-contents)
  (package-install 'org-plus-contrib))

(eval-when-compile
  (require 'use-package))
(require 'diminish) ; if you use :diminish
(require 'bind-key) ; if you use any :bind variant
(require 'cl-lib) ; require common lisp expressions
;(require 'cl) ; require common lisp

;; load up all literate org-mode files in this directory
(require 'org)
(require 'org-install)
(require 'ob-tangle)
(require 'ox)

(mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$"))

(setq gc-cons-threshold 800000)


;;; init.el ends here
