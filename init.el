;;; init.el --- Where all the magic begins

(when (version< emacs-version "25.3") ; Minimum version
  (error "Your Emacs is too old -- this config requires v25.3 or higher"))

;; This file loads Org-mode and then loads the rest of our Emacs
;; initialization from Emacs lisp embedded in emacs.org.

(setq gc-cons-threshold 20000000)

;; add MELPA, Org, and ELPY
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; Get the latest version of org from gnu elpa:
(unless (package-installed-p 'org (version-to-list "9.1"))
  (package-refresh-contents)
  (package-install (cadr (assq 'org package-archive-contents))))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; load up all literate org-mode files in this directory
(require 'org)

(org-babel-load-file (concat user-emacs-directory "emacs.org"))

(setq gc-cons-threshold 800000)


;;; init.el ends here
