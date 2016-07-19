;;; init.el --- Where all the magic begins

(let ((minver 24)); Minimum version is emacs 24
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;; This file loads Org-mode and then loads the rest of our Emacs
;; initialization from Emacs lisp embedded in emacs.org.

(setq gc-cons-threshold 20000000)

;; add MELPA, Org, and ELPY
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("elpy" . "https://jorgenschaefer.github.io/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))
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
