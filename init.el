;;; init.el --- Where all the magic begins

(when (version< emacs-version "25.3") ; Minimum version
  (error "Your Emacs is too old -- this config requires v25.3 or higher"))

;; This file loads Org-mode and then loads the rest of our Emacs
;; initialization from Emacs lisp embedded in emacs.org.

(setq gc-cons-threshold 20000000)

;; Set up package.el, Emacs's built-in package manager. I have to set this up
;; here instead of emacs.org because Emacs complains otherwise. Loudly.

;; Eventually, I'd like to switch to something like
;; [[borg][https://github.com/emacscollective/borg]], but haven't found the time
;; yet.
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
;; Don't save packages in init file
(defun package--save-selected-packages (&optional VALUE opt)
  nil)
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
(add-to-list 'package-selected-packages 'use-package)
;; Since I redefined `package--save-selected-packages' to do nothing, I need to
;; tell use-package's :ensure to save packages to package-selected-packages.
(defun my/use-package-ensure-elpa (name ensure state context &optional no-refresh)
  (let ((package (or (when (eq ensure t) (use-package-as-symbol name))
                     ensure)))
    (when package
      (add-to-list 'package-selected-packages package)))
  (use-package-ensure-elpa name ensure state context no-refresh))

(setq use-package-ensure-function #'my/use-package-ensure-elpa)

;; load up all literate org-mode files in this directory
(require 'org)

(org-babel-load-file (concat user-emacs-directory "emacs.org"))

(setq gc-cons-threshold 800000)


;;; init.el ends here
