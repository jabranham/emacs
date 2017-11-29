;;; init.el --- user-init-file                    -*- lexical-binding: t -*-

(when (version< emacs-version "25.3") ; Minimum version
  (error "Your Emacs is too old -- this config requires v25.3 or higher"))

;;; Early birds
(progn ;     startup
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (setq package-enable-at-startup nil)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (advice-add #'display-startup-echo-area-message :override #'ignore)
  (setq load-prefer-newer t)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0))

(progn ;    `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require  'borg)
  (borg-initialize))

(progn ;    `use-package'
  (setq use-package-enable-imenu-support t)
  (setq use-package-verbose t)
  (require 'use-package))

(use-package auto-compile
  :demand t
  :hook
  (auto-compile-inhibit-compile . auto-compile-inhibit-compile-detached-git-head)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t))

(use-package epkg
  :defer t
  :bind
  ("C-h P" . epkg-describe-package)
  :init (setq epkg-repository
              (expand-file-name "var/epkgs/" user-emacs-directory)))

(org-babel-load-file (concat user-emacs-directory "emacs.org"))

(add-hook 'emacs-startup-hook (lambda () (message (concat "Booted in: " (emacs-init-time)))))

;; Local Variables:
;; indent-tabs-mode: nil
;; no-byte-compile: t
;; End:

;;; init.el ends here
