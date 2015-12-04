;;; init.el --- my emacs configuration

;;; Commentary:
; This is how I set up my emacs
; See https://github.com/jabranham/emacs for a readme on how to get set up


;;; Code:
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
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
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


;; (use-package ido-ubiquitous
;;   :ensure t
;;   :config
;;   (use-package smex
;;     :ensure t
;;     :bind
;;     ("M-x" . smex)
;;     ("C-c C-c M-x" . execute-extended-command)
;;     ("M-X" . smex-major-mode-commands))
;;   (use-package ido-vertical-mode
;;     :ensure t
;;     :config
;;     (ido-mode 1) ; turn on ido mode
;;     (ido-vertical-mode 1) ; turn on ido vertical mode
;;     (setq ido-vertical-define-keys 'C-n-C-p-up-and-down))) ; make up and down keys work

(use-package helm
  :ensure t
  :bind
  ("C-c h" . helm-command-prefix)
  ("M-x" . helm-M-x)
  ("M-y" . helm-show-kill-ring)
  ("C-x b". helm-mini)
  ("C-x C-f". helm-find-files)
  ("C-c h g". helm-google-suggest)
  ("C-c h c" . helm-calcul-expression)
  :init
  (require 'helm-config)
  :config
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-M-x-fuzzy-match                  t ; optional fuzzy matching for helm-M-x
        helm-autoresize-mode                  t
        helm-buffers-fuzzy-matching           t
        helm-recentf-fuzzy-match              t)
  (helm-mode t))
(use-package projectile
  :ensure t)
(use-package helm-projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))
(use-package helm-R
  :ensure t)
(use-package helm-flyspell
  :ensure t
  :config
  (define-key flyspell-mode-map (kbd "M-/") 'helm-flyspell-correct))
(use-package helm-company
  :ensure t
  :config
  (eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-;") 'helm-company)
     (define-key company-active-map (kbd "C-;") 'helm-company))))
(use-package helm-flycheck
  :ensure t
  :config
  (eval-after-load 'flycheck
   '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck)))

(use-package smartparens ; makes parens easier to keep track of
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode +1))

(use-package markdown-mode ; for markdown mode
  :ensure t)

(use-package ess-site ; for R goodness
  :ensure ess
  :config
  (add-hook 'ess-mode-hook
            (lambda ()
              (ess-set-style 'RStudio)))
  (setq ess-offset-arguments 'prev-line))

(use-package stan-mode
  :ensure t)
  ;; :config
  ;; (use-package stan-snippets
  ;;   :ensure t))

(use-package auctex ; for LaTeX documents
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :init
  (progn
    (add-hook 'LaTeX-mode-hook 'visual-line-mode)
    ;(add-hook 'LaTeX-mode-hook 'flyspell-mode)
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
  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list '("latexmk" "latexmk -synctex=1 -shell-escape -pdf %s" TeX-run-TeX nil t :help "Process file with latexmk"))
    )
  (eval-after-load "tex"
    '(add-to-list 'TeX-command-list '("xelatexmk" "latexmk -synctex=1 -shell-escape -xelatex %s" TeX-run-TeX nil t :help "Process file with xelatexmk"))
    )
  (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))  
  )
;; (use-package auctex-latexmk ; enables latexmk
;;   :ensure t
;;   :config
;;   (auctex-latexmk-setup)
;;   (setq auctex-latexmk-inherit-TeX-PDF-mode t)
;;   (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "LatexMk"))))

(when (eq system-type 'gnu/linux) 
  (pdf-tools-install) ; nice PDF viewer (needs separate installation)
  (setq TeX-view-program-selection '((output-pdf "pdf-tools")))
  (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))) ; set up pdf-tools as pdf viewer
 
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
  :init
  ;; Make RefTeX work with Org-Mode
  ;; use 'C-c (' instead of 'C-c [' because the latter is already
  ;; defined in orgmode to the add-to-agenda command.
  (defun org-mode-reftex-setup ()
    (load-library "reftex") 
    (and (buffer-file-name)
         (file-exists-p (buffer-file-name))
         (reftex-parse-all))
    (define-key org-mode-map (kbd "C-c (") 'reftex-citation))
  (add-hook 'org-mode-hook 'org-mode-reftex-setup)
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
        (?X . "{%l}")))
  ;; So that RefTeX also recognizes \addbibresource. Note that you
  ;; can't use $HOME in path for \addbibresource but that "~"
  ;; works.
  (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
  (setq reftex-default-bibliography '("~/Dropbox/library.bib"))
  (setq reftex-extra-bindings t)
  )

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
  (setq ispell-program-name "aspell")
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (add-hook 'text-mode-hook 'turn-on-flyspell)
  (add-hook 'LaTeX-mode-hook 'turn-on-flyspell)
  (add-hook 'markdown-mode-hook 'turn-on-flyspell)
  (add-hook 'org-mode-hook 'turn-on-flyspell))


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
  :ensure t
  :config
  (setq dired-dwim-target t))

(use-package rainbow-delimiters ; for nice coloring of parens
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'ess-mode-hook #'rainbow-delimiters-mode))

;; (use-package smart-mode-line
;;   :ensure t
;;   :config
;;   (sml/setup))

(use-package org
  :ensure org-plus-contrib
  :config
  (setq org-src-fontify-natively     t
        org-src-tab-acts-natively    t
        org-confirm-babel-evaluate   nil
        org-pretty-entities          t
        org-support-shift-select     t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (emacs-lisp . t)
     (latex . t)
     (python . t)))
  (add-to-list 'org-src-lang-modes
               '("r" . ess-mode))
  ;(require 'ob-latex)
  ;; use latexmk
  (add-to-list 'org-babel-noweb-error-langs "latex")
  ;(setq org-latex-to-pdf-process "latexmk -f -pdf %f") ; for org version < 8.0
  (setq org-latex-pdf-process (list "latexmk -f -pdf %f"))
  ;; when working via C-c ' open in current window
  (setq org-src-window-setup 'current-window)
  ;; display inline images
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)   
  (add-hook 'org-mode-hook 'org-display-inline-images)
)
(use-package htmlize
  :ensure t)

;; (use-package shell-pop
;;   :ensure t
;;   :bind
;;   ("C-c C-t" . shell-pop)
;;   :config
;;   (setq shell-pop-shell-type '("eshell")))

(use-package buffer-move
  :ensure t
  :bind
  ("M-S-<up>" . buf-move-up)
  ("M-S-<down>" . buf-move-down)
  ("M-S-<left>" . buf-move-left)
  ("M-S-<right>" . buf-move-right)
  :config
  (setq buffer-move-behavior 'move))

(use-package mu4e
  :config
  ;; default
  (setq mu4e-maildir "~/Documents/Maildir/utexas")
  (setq mu4e-drafts-folder "/[Gmail].Drafts")
  (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
  (setq mu4e-trash-folder  "/[Gmail].Trash")
  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)
  (setq
   mu4e-get-mail-command "offlineimap"   ;; or fetchmail, or ...
   mu4e-update-interval 180)             ;; update every 3 minutes
  ;; setup some handy shortcuts
  ;; you can quickly switch to your Inbox -- press ``ji''
  ;; then, when you want archive some messages, move them to
  ;; the 'All Mail' folder by pressing ``ma''.
  (setq mu4e-maildir-shortcuts
        '( ("/INBOX"  . ?i)
           ("/[Gmail].Sent Mail"   . ?s)
           ("/[Gmail].Trash"  . ?t)
           ("/[Gmail].All Mail" . ?a)))
  ;; something about ourselves
  (setq mu4e-user-mail-address-list '("branham@utexas.edu"))
  (setq
   user-mail-address "branham@utexas.edu"
   user-full-name  "Alex Branham")
  (setq mu4e-compose-signature
        (concat
         "J. Alexander Branham\n"
         "PhD Candidate\n"
         "Department of Government\n"
         "University of Texas at Austin"
         "\n"))
  (setq mu4e-compose-dont-reply-to-self t) ; don't reply to self
  (setq mu4e-compose-complete-only-after "2015-01-01")
  ;; enable inline images
  (setq mu4e-view-show-images t)
  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)
  (add-hook 'mu4e-view-mode-hook 'visual-line-mode)
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-headers-skip-duplicates t)
  ;; attachments go here
  (setq mu4e-attachment-dir "~/Downloads")
  (use-package gnus-dired
    ;; make the `gnus-dired-mail-buffers' function also work on
    ;; message-mode derived modes, such as mu4e-compose-mode
    :config
    (defun gnus-dired-mail-buffers ()
      "Return a list of active message buffers."
      (let (buffers)
        (save-current-buffer
          (dolist (buffer (buffer-list t))
            (set-buffer buffer)
            (when (and (derived-mode-p 'message-mode)
                       (null message-sent-message-via))
              (push (buffer-name buffer) buffers))))
        (nreverse buffers)))
    (setq gnus-dired-mail-mode 'mu4e-user-agent)
    (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode))
  ;; configure orgmode support in mu4e
  (use-package org-mu4e
  ;; when mail is sent, automatically convert org body to HTML
    :config
    (setq org-mu4e-convert-to-html t))
  ;; need to do org-mu4e-compose-org-mode
  ;; and include #+OPTIONS: tex:imagemagick
  ;; then send while in headers for this to work properly 
  ;; Start mu4e in fullscreen
  (defun my-mu4e-start ()
    (interactive)
    (window-configuration-to-register :mu4e-fullscreen)
    (mu4e)
    (delete-other-windows))
  ;; Restore previous window configuration
  (defun mu4e-quit-session ()
    "Restores the previous window configuration and kills the mu4e buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :mu4e-fullscreen))
  (define-key mu4e-main-mode-map (kbd "q") 'mu4e-quit-session)
  (global-set-key (kbd "<f1>") 'my-mu4e-start)
  (global-set-key (kbd "<f2>") 'mu4e-compose-new)
  (mu4e t) ; starts mu4e when emacs starts, but silently
  (use-package mu4e-contrib
    :config
    ;; html2text command from eww browser
    (setq mu4e-html2text-command 'mu4e-shr2text)
    ;; use aV to open message in browser
    (add-to-list 'mu4e-view-actions
                 '("ViewInBrowser" . mu4e-action-view-in-browser) t))
  )
(use-package helm-mu
  :ensure t)

(use-package smtpmail
  :config
  (setq message-send-mail-ggfunction 'smtpmail-send-it
        smtpmail-stream-type 'starttls
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587))
   

(use-package mu4e-alert
  :ensure t
  :config
  ;; Choose the style you prefer for desktop notifications
  ;; If you are on Linux you can use
  ;; 1. notifications - Emacs lisp implementation of the Desktop Notifications API
  ;; 2. libnotify     - Notifications using the `notify-send' program, requires `notify-send' to be in PATH
  ;;
  ;; On Mac OSX you can set style to
  ;; 1. notifier      - Notifications using the `terminal-notifier' program, requires `terminal-notifier' to be in PATH
  ;; 1. growl         - Notifications using the `growl' program, requires `growlnotify' to be in PATH
  (mu4e-alert-set-default-style 'libnotify)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  (setq mu4e-alert-interesting-mail-query
        (concat
         "flag:unread"
         " AND NOT flag:trashed"
         " AND NOT maildir:"
         "\"/[Gmail].All Mail\"")))

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

;; make splitting windows behave more like I want it to
(defun my/vsplit-last-buffer (prefix)
  "Split the window vertically and display the previous buffer."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (if (= prefix 1)
    (switch-to-next-buffer)))
(defun my/hsplit-last-buffer (prefix)
  "Split the window horizontally and display the previous buffer."
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (if (= prefix 1) (switch-to-next-buffer)))
(bind-key "C-x 2" 'my/vsplit-last-buffer)
(bind-key "C-x 3" 'my/hsplit-last-buffer)

;; misc settings
(global-set-key (kbd "C-z") 'undo) ; set "C-z" to undo, rather than minimize emacs (which seems useless)
(define-key global-map (kbd "C-+") 'text-scale-increase) ; C-+ increases font size
(define-key global-map (kbd "C--") 'text-scale-decrease) ; C-- decreases font size
(if window-system ; show menu if emacs is window, not if terminal
    (menu-bar-mode t)
    (menu-bar-mode -1))
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
;; change frame name of emacs
(setq frame-title-format
  '("Emacs - " (buffer-file-name "%f"
    (dired-directory dired-directory "%b"))))

(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)

(defun sanityinc/kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(bind-key "C-M-<backspace>" 'sanityinc/kill-back-to-indentation)

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
     (ess-R-fl-keyword:%op% . t))))
 '(org-agenda-files nil)
 '(send-mail-function (quote smtpmail-send-it)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
