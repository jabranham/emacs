;;; init.el --- Where all the magic begins

; Minimum version is emacs 24
(let ((minver 24))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;; This file loads Org-mode and then loads the rest of our Emacs
;; initialization from Emacs lisp embedded in emacs.org.

(setq gc-cons-threshold 20000000)

;; add MELPA, Org, and ELPY
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(package-initialize)

;; Bootstrap 'use-package' and 'org-mode
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(unless (package-installed-p 'org-plus-contrib)
  (package-refresh-contents)
  (package-install 'org-plus-contrib))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; load up all literate org-mode files in this directory
(require 'org)

(org-babel-load-file (concat user-emacs-directory "emacs.org"))

(setq gc-cons-threshold 800000)


;;; init.el ends here
