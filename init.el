;;; init.el --- Where all the magic begins

(if (version< emacs-version "25.1") ; Minimum version 
    (error "Your Emacs is too old -- this config requires v25.1 or higher"))

;; This file loads Org-mode and then loads the rest of our Emacs
;; initialization from Emacs lisp embedded in emacs.org.

(setq gc-cons-threshold 20000000)

;; add MELPA, Org, and ELPY
(require 'package)
(setq package-archives
      '(("elpy" . "https://jorgenschaefer.github.io/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
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
