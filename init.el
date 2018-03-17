;;; init.el --- Emacs initialization  -*- lexical-binding: t; -*-


;;; Commentary:
;; This is my personal Emacs config.  It works for me, but probably won't
;; for you.

;;; Code:

(when (version< emacs-version "26") ; Minimum version
  (error "Your Emacs is too old -- this config requires version 26 or higher"))

;;; Early birds
(progn ;     startup & C source code vars
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (setq package-enable-at-startup nil)
  ;; (package-initialize)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (advice-add #'display-startup-echo-area-message :override #'ignore)
  (setq load-prefer-newer t)
  ;; don't use popup boxes, just make the minibuffer ask
  (setq use-dialog-box nil)
  (setq initial-major-mode #'org-mode
        initial-scratch-message "# Unsaved notes\n\n")
  ;; Delete my files by moving them to the trash. I'm human and
  ;; occasionally delete things that I actually want later:
  (setq delete-by-moving-to-trash t)
  ;; Emacs has some awful scrolling by default. This gets rid of that.
  (setq scroll-step 1) ; keyboard scroll one line at a time
  (setq scroll-preserve-screen-position t)
  (setq scroll-conservatively 101)
  (setq next-screen-context-lines 5)
  ;; Don't ever use tabs. Always use spaces.
  (setq-default indent-tabs-mode nil)
  ;; for the lazy:
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; remove visual clutter:
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  ;; remove auditory clutter:
  (setq ring-bell-function 'ignore)
  ;; Emacs thinks that some new users may find some commands confusing, so
  ;; they're disabled by default. I use these every now and then, so let's
  ;; enable them by default:
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  ;; Prefer utf8
  (prefer-coding-system 'utf-8))

(progn ;    `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require  'borg)
  (borg-initialize))

(progn ;    `use-package'
  (setq use-package-enable-imenu-support t)
  (require 'use-package)
  (setq use-package-compute-statistics t)
  ;; Setup a personal keymap. I'll bind various things to this later on:
  (bind-keys :prefix "<f1>"
             :prefix-map my/map))


(use-package auto-compile
  :demand t
  :custom
  (auto-compile-mode-line-counter t "Show compile info in the mode-line")
  (auto-compile-source-recreate-deletes-dest t)
  (auto-compile-toggle-deletes-nonlib-dest t)
  (auto-compile-update-autoloads t)
  (auto-compile-display-buffer nil "Don't display compile buffer")
  :hook
  (auto-compile-inhibit-compile . auto-compile-inhibit-compile-detached-git-head)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; Finally, I set up no-littering, which keeps my .emacs.d folder clean by
;; putting files into appropriate subfolders rather than letting them get
;; saved all over the place:
(use-package no-littering
  :demand t)

;;; end of early birds, alphabetical from here on out:

(use-package abbrev
  :custom
  (save-abbrevs 'silently)
  :hook
  (text-mode . abbrev-mode))

(use-package aggressive-indent
  ;; Keep code indented automatically
  :defer 10
  :config
  (global-aggressive-indent-mode))

(use-package alert
  ;; Set it up so Emacs can send system notifications:
  :defer t
  :custom
  (alert-default-style 'libnotify)
  :config
  (defun my/pause-notifications ()
    "Pause notification display."
    (interactive)
    (shell-command "killall -SIGUSR1 dunst" nil nil)
    (message "Notifications paused."))
  (defun my/resume-notifications ()
    "Resume notification display."
    (interactive)
    (shell-command "killall -SIGUSR2 dunst" nil nil)
    (message "Notifications resumed.")))

(use-package anaconda-mode
  ;; sets up some nice things in python buffers:
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode))

(use-package appt
  ;; keep track of appointments
  :hook
  (after-init . appt-activate)
  :custom
  (appt-delete-window-function (lambda () t))
  (appt-disp-window-function #'my/appt-display)
  (appt-display-interval 12 "Don't notify more than once")
  (appt-message-warning-time 12)
  (appt-display-mode-line nil)
  :config
  (defun my/appt-display (time-til _time msg)
    (if (listp time-til)
        (dotimes (i (length msg))
          (alert (concat (nth i msg) " in " (nth i time-til) " minutes")
                 :title "Appt"))
      (alert (concat msg " in " time-til " minutes") :title "Appt"))))

(use-package async
  ;; Async is written to let things be more async-y in Emacs.  I use it for
  ;; dired-async mode mostly.
  :custom
  (dired-async-message-function #'my/dired-async-message-function)
  :config
  (defun my/dired-async-message-function (text _face &rest args)
    "Log messages from dired-async to messages buffer."
    ;; For whatever reason, the default for this *doesn't* log it to
    ;; *Messages*.  Instead, it just displays the notification in the
    ;; mode line for 3 seconds, but if you type something it
    ;; immediately goes away.  So just log it to *Messages* like a sane
    ;; person instead:
    (message (format "Finished %s" (apply #'format text args))))
  ;; do dired actions asynchronously
  (dired-async-mode))

(use-package auth-source-pass
  ;; Integrate Emacs's builtin auth-source with pass:
  :if (executable-find "pass")
  :demand t
  :config
  (auth-source-pass-enable))

(use-package autorevert
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  :config
  ;; Emacs should refresh buffers automatically so if they've changed on
  ;; disk the buffer will update.
  (global-auto-revert-mode))

(use-package bash-completion
  ;; We can set it up so that we get pretty good bash completion in
  ;; shell-mode and eshell.  Note that for this to work, you'll need
  ;; bash-completion installed.
  :custom
  (bash-completion-nospace t)
  :config
  (defun eshell-bash-completion ()
    (while (pcomplete-here
            (nth 2 (bash-completion-dynamic-complete-nocomint
                    (save-excursion
                      (eshell-bol) (point))
                    (point))))))
  (bash-completion-setup))

(use-package bibtex
  :defer t ; built-in with Emacs
  :custom
  (bibtex-autokey-titleword-length 0)
  (bibtex-autokey-titleword-separator "")
  (bibtex-autokey-titlewords 0)
  (bibtex-autokey-year-length 4)
  (bibtex-autokey-year-title-separator "")
  (bibtex-align-at-equal-sign t)
  ;; The default for bibtex-entry-format includes opts-or-alts, which deletes
  ;; empty entries. I want to keep those around, though, because a lot of
  ;; forthcoming articles get things like pages later:
  (bibtex-entry-format '(required-fields numerical-fields))
  (bibtex-files '("~/Sync/bibliography/references.bib"))
  :hook
  (bibtex-mode . my/setup-bibtex-mode)
  :config
  (defun my/setup-bibtex-mode ()
    "Set up bibtex mode."
    (set-fill-column most-positive-fixnum))
  (defun my/bibtex-generate-autokey ()
    "This overwrites the bibtex-generate-autokey function that comes with Emacs.

  I want my keys to be formatted: authornameYEAR, then a letter if there is already an entry that matches authornameYEAR."
    (interactive)
    ;; first we delete the existing key
    (bibtex-beginning-of-entry)
    (re-search-forward bibtex-entry-maybe-empty-head)
    (if (match-beginning bibtex-key-in-head)
        (delete-region (match-beginning bibtex-key-in-head)
                       (match-end bibtex-key-in-head)))
    (let* ((names (bibtex-autokey-get-names))
           (year (bibtex-autokey-get-year))
           (existing-keys (bibtex-parse-keys))
           key)
      (setq key (format "%s%s" names year))
      (let ((ret key))
        (cl-loop for c
                 from ?b to ?z
                 while (assoc ret existing-keys)
                 do (setq ret (format "%s%c" key c)))
        ret)))
  (advice-add #'bibtex-generate-autokey :override #'my/bibtex-generate-autokey)
  ;; the built-in bibtex-mark-entry function does not activate the mark.
  ;; I've submitted a patch that fixes that, hopefully it makes it into
  ;; Emacs 26.
  (defun my/bibtex-mark-entry ()
    "Put mark at beginning, point at end of current BibTeX entry."
    (interactive)
    (push-mark (bibtex-beginning-of-entry) :activate t)
    (bibtex-end-of-entry))
  (advice-add #'bibtex-mark-entry :override #'my/bibtex-mark-entry))

(use-package browse-url
  :custom
  ;; Use Emacs' built in eww broswer (the Emacs Web Wowser!) by default.
  ;; browse-url-browser-function can take a list of regex's and associate a
  ;; specific browser with matches.  So use eww for everything except a few
  ;; things that don't work well:
  (browse-url-browser-function
   '((".*login.utexas.*" . browse-url-firefox)
     (".*utdirect.*utexas.*" . browse-url-firefox)
     (".*reddit.*" . browse-url-firefox)
     (".*github.*" . browse-url-firefox)
     (".*youtube.*" . browse-url-firefox)
     (".*youtu.be*" . browse-url-firefox)
     (".apsanet.*" . browse-url-firefox)
     (".interfolio.*" . browse-url-firefox)
     (".academicjobsonline.*" . browse-url-firefox)
     (".accounts.google.com*" . browse-url-firefox)
     ("." . eww-browse-url))))

(use-package calc
  :defer t
  :bind
  (:map my/map
        ("C" . my/calc-eval-region))
  :config
  (defun my/calc-eval-region (arg)
    "Evaluate an expression in calc and communicate the result.

If the region is active evaluate that, otherwise search backwards
to the first whitespace character to find the beginning of the
expression. By default, replace the expression with its value. If
called with the universal prefix argument, keep the expression
and insert the result into the buffer after it. If called with a
negative prefix argument, just echo the result in the
minibuffer."
    (interactive "p")
    (let (start end)
      (if (use-region-p)
          (setq start (region-beginning) end (region-end))
        (progn
          (setq end (point))
          (setq start (search-backward-regexp "\\s-\\|\n" 0 1))
          (setq start (1+ (if start start 0)))
          (goto-char end)))
      (let ((value (calc-eval (buffer-substring-no-properties start end))))
        (pcase arg
          (1 (delete-region start end))
          (4 (insert " = ")))
        (pcase arg
          ((or 1 4) (insert value))
          (-1 (message value)))))))

(use-package calendar
  ;; Yes, my text editor comes with a calendar built in.  Doesn't yours?
  :defer t
  :custom
  (calendar-location-name "Austin")
  (calendar-latitude [30 16 north])
  (calendar-longitude [97 44 west])
  (calendar-mark-holidays-flag t "Show holidays in the calendar")
  (calendar-week-start-day 0 "Weeks start on Sunday")
  :hook
  ;; make today easier to find, visually:
  (calendar-today-visible . calendar-mark-today)
  :config
  (setq calendar-date-display-form calendar-iso-date-display-form)
  (calendar-set-date-style 'iso))

(use-package comint
  ;; comint is the mode from which inferior processes inherit, like the
  ;; python REPL or iESS modes (the R console)
  :custom
  (comint-move-point-for-output nil)
  (comint-scroll-to-bottom-on-input 'this))

(use-package company
  ;; Company mode provides autocompletion of text and code.
  :bind
  (:map company-active-map
        ("C-s" . company-search-candidates)
        ("<tab>" . company-complete-common-or-cycle)
        ("RET" . company-complete-selection)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  :hook
  ((prog-mode ess-mode) . company-mode)
  :custom
  (company-idle-delay 0.25)
  (company-require-match nil)
  (company-minimum-prefix-length 2))

(use-package company-anaconda
  ;; company for integration with anaconda (loaded above)
  :after (anaconda-mode company)
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package company-childframe
  :defer 10
  :config
  (company-childframe-mode))

(use-package compile
  :defer t
  :custom
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error))

(use-package conf-mode
  :mode (("\\.service\\'" . conf-unix-mode)
         ("\\.timer\\'" . conf-unix-mode)
         ("\\.target\\'" . conf-unix-mode)
         ("\\.mount\\'" . conf-unix-mode)
         ("\\.automount\\'" . conf-unix-mode)
         ("\\.slice\\'" . conf-unix-mode)
         ("\\.socket\\'" . conf-unix-mode)
         ("\\.path\\'" . conf-unix-mode)
         ("\\.netdev\\'" . conf-unix-mode)
         ("\\.network\\'" . conf-unix-mode)
         ("\\.link\\'" . conf-unix-mode)
         ("\\.automount\\'" . conf-unix-mode)))

(use-package csv-mode
  ;; Emacs can handle csv files with ease:
  :mode (("\\.csv" . csv-mode)))

(use-package delsel
  :config
  ;; Emacs by default doesn't replace selected text if you start typing
  ;; over it.  Since that's the behavior of virtually all other programs,
  ;; let's make emacs do that too:
  (delete-selection-mode))

(use-package diff-hl
  :defer 15
  ;; highlight changes to files on the side
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode))

(use-package dired
  ;; Emacs can act as your file finder/explorer.  Dired is the built-in way
  ;; to do this.
  :defer t
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  ;; -l: long listing format REQUIRED in dired-listing-switches
  ;; -a: show everything (including dotfiles)
  ;; -h: human-readable file sizes
  (dired-listing-switches "-alh --group-directories-first")
  :bind
  (("C-x C-d" . dired) ; overrides list-directory, which I never use
   :map  dired-mode-map
   ("l" . dired-up-directory)) ; use l to go up in dired
  :config
  (defun my/dired-ediff-marked ()
    "Run `ediff' on two marked files in a dired buffer."
    (interactive)
    (unless (eq 'dired-mode major-mode)
      (error "For use in dired buffers only"))
    (let ((files (dired-get-marked-files)))
      (when (not (eq 2 (length files)))
        (error "Two files not marked"))
      (ediff (car files) (nth 1 files)))))

(use-package dired-du
  ;; List directory sizes using du:
  :bind
  (:map dired-mode-map
        ("S" . dired-du-mode))
  :custom
  (dired-du-size-format t)
  :hook
  (dired-mode . my/dired-maybe-hide-details)
  :config
  (defun my/dired-maybe-hide-details ()
    "Hide details (owner, permissions, etc) in dired unless dired-du-mode is active."
    (unless dired-du-mode (dired-hide-details-mode))))

(use-package dired-x
  :hook
  (dired-load . (lambda () (load "dired-x")))
  :bind
  ("C-x C-j" . dired-jump)
  :custom
  ;; By default, dired asks you if you want to delete the dired buffer if
  ;; you delete the folder. I can't think of a reason I'd ever want to do
  ;; that.
  (dired-clean-confirm-killing-deleted-buffers nil))

(use-package ediff
  ;; Ediff is great, but I have to tell it to use one frame (since I start
  ;; Emacs before X/wayland, it defaults to using two frames).
  :defer t
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  :hook
  (ediff-prepare-buffer . my/ediff-prepare-buffer)
  :config
  (defun my/ediff-prepare-buffer ()
    "Function to prepare ediff buffers.

Runs with `ediff-prepare-buffer-hook' so that it gets run on all
three ediff buffers (A, B, and C)."
    (when (memq major-mode '(org-mode emacs-lisp-mode))
      ;; unfold org/elisp files
      (outline-show-all))))

(use-package edit-indirect
  ;; Markdown relies on this package for to edit source code blocks like
  ;; org mode:
  :defer t)

(use-package eldoc
  ;; eldoc shows useful information in the minibuffer and is enabled by default.
  :defer t
  :custom
  (eldoc-idle-delay 0 "We can speed it up a bit."))

(use-package elec-pair
  :custom
  (electric-pair-mode t))

(use-package electric-operator
  ;; Electric operator will turn ~a=10*5+2~ into ~a = 10 * 5 + 2~, so let's
  ;; enable it for R:
  :hook
  ((ess-mode python-mode) . electric-operator-mode)
  :custom
  (electric-operator-R-named-argument-style 'spaced))

(use-package elfeed
  ;; Manage RSS and atom feeds from within Emacs!
  :bind
  (:map my/map
        ("s" . bjm/elfeed-load-db-and-open)
        :map elfeed-search-mode-map
        ("l" . my/get-elfeed-log-buffer))
  :custom
  (elfeed-db-directory "~/Sync/.elfeed")
  (elfeed-search-print-entry-function #'my/elfeed-print-entry)
  :init
  ;; thanks -
  ;; http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed/
  ;; though slightly modified functions to support syncing .elfeed between
  ;; machines makes sure elfeed reads index from disk before launching
  (defun bjm/elfeed-load-db-and-open ()
    "Load the elfeed db from disk before updating."
    (interactive)
    (elfeed)
    (elfeed-db-load)
    (elfeed-search-update--force)
    (elfeed-update))
  :hook
  (elfeed-show-mode . my/setup-elfeed-show-mode)
  (elfeed-search-mode . my/setup-elfeed-search-mode)
  :config
  (defun my/setup-elfeed-show-mode ()
    "Setup `elfeed-show-mode'."
    (setq-local shr-width 80)
    ;; Scale down huge images:
    (setq-local shr-max-image-proportion 0.6))
  (defun my/setup-elfeed-search-mode ()
    "Setup `elfeed-search-mode'."
    ;; Don't use visual line mode in elfeed-search:
    (visual-line-mode -1))
  ;; Overwrite the default print-entry function with one that prints date,
  ;; then feed-title, then title:
  (defun my/elfeed-print-entry (entry)
    "Print ENTRY to the buffer."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
           (title (or (elfeed-meta entry :title)
                      (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (title-column (elfeed-format-column
                          title (+ (window-width) (- 12) (- 12)) :left))
           (feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (feed-column (elfeed-format-column
                         feed-title 10 :right)))
      (insert (propertize date 'face 'elfeed-search-date-face) " ")
      (when feed-title
        (insert (propertize feed-column 'face 'elfeed-search-feed-face) " "))
      (insert (propertize title-column 'face title-faces) " ")))
  (defun my/get-elfeed-log-buffer ()
    "Show elfeed log."
    (interactive)
    (switch-to-buffer-other-window (get-buffer "*elfeed-log*")))
  (use-package elfeed-link))

(use-package elfeed-org
  :after elfeed
  :custom
  (rmh-elfeed-org-files '("~/Sync/.elfeed/rmh-elfeed.org"))
  :config
  (elfeed-org))

(use-package elisp-mode
  :defer t
  :hook
  ;; Turn on flymake for emacs-lisp:
  (emacs-lisp-mode . my/setup-emacs-lisp-mode)
  :config
  (defun my/setup-emacs-lisp-mode ()
    "Setup stuff for elisp."
    ;; Sentences end with a double space in elisp:
    (setq-local sentence-end-double-space t)))

(use-package emacsbug
  :defer t
  :custom
  (report-emacs-bug-no-explanations t))

(use-package epa
  ;; EasyPG Assistant for encryption
  :custom
  (epa-pinentry-mode 'loopback))

(use-package epkg
  :defer t
  :bind
  ("C-h P" . epkg-describe-package)
  :custom
  (epkg-repository (expand-file-name "var/epkgs/" user-emacs-directory)))

(use-package erc
  ;; ERC is Emacs's client for IRC.
  :if (executable-find "pass")
  :commands (erc)
  :custom
  (erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#archlinux")))
  (erc-join-buffer 'bury)
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-password (password-store-get "irc.freenode.net"))
  (erc-port "6667")
  (erc-server "irc.freenode.net")
  (erc-server-reconnect-attempts 12)
  (erc-server-reconnect-timeout 5)
  (erc-nick "jabranham")
  :hook
  (erc-mode . goto-address-mode)
  (erc-mode . erc-notifications-mode))

(use-package eshell
  ;; Eshell is Emacs' built-in shell.  You get UNIX-y goodness even on
  ;; Windows machines, plus it can evaluate elisp.
  :defer t
  :custom
  (eshell-buffer-maximum-lines 20000 "Auto truncate after 20k lines")
  (eshell-highlight-prompt nil "My prompt is easy enough to see")
  (eshell-hist-ignoredups t "No duplicates in history")
  (eshell-history-size 1024 "history size")
  (eshell-list-files-after-cd t "List files after cd.")
  (eshell-ls-initial-args "-ah" "Also list all files & human-readable filesizes.")
  (eshell-plain-echo-behavior t "treat 'echo' like shell echo")
  (eshell-prompt-function #'my/eshell-prompt)
  (eshell-prompt-regexp "^λ ")
  (eshell-scroll-to-bottom-on-input 'this)
  (eshell-cmpl-cycle-completions nil)
  :hook
  ;; Make urls clickable
  (eshell-mode . goto-address-mode)
  (eshell-mode . my/setup-eshell)
  :bind
  ("C-c M-e" . eshell)
  ("C-c C-M-e" . my/eshell-remote)
  :config
  (defun my/eshell-remote (host)
    "Open eshell on a remote host.

Uses `pcmpl-ssh-config-hosts' to obtain a list of possible hosts."
    (interactive
     (list
      (completing-read "Host: " (pcmpl-ssh-config-hosts))))
    (eshell)
    (setq default-directory (concat "/ssh:" host ":"))
    (eshell-reset))
  (defun my/setup-eshell ()
    "Set up eshell how I want.  To be called by `eshell-mode-hook'."
    (progn
      (eshell-cmpl-initialize)
      (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
      (unbind-key "M-s" eshell-mode-map)
      (bind-key "M-r" #'helm-eshell-history eshell-mode-map)))
  (use-package pcomplete-extension
    :demand t)
  (defun my/eshell-prompt ()
    "Function that determines the eshell prompt.  Must set
`eshell-prompt-function' to this for it to work."
    (let ((path (abbreviate-file-name (eshell/pwd))))
      (concat
       ;; working directory
       (format (propertize "(%s)")
               (propertize path 'face '(:foreground "green")))
       ;; git info
       (when (and (fboundp #'magit-get-current-branch) ; magit might not be loaded yet
                  (magit-get-current-branch))
         (format (propertize "@%s")
                 (propertize (magit-get-current-branch) 'face '(:foreground "orange"))))
       ;; newline, then prompt
       (propertize "\nλ" 'face '(:weight bold))
       ;; need to have a space, otherwise the first text I type gets
       ;; propertized to match λ:
       " "))))

(use-package esh-module
  :defer t
  :config
  ;; Don't show the welcome message banner:
  (delq 'eshell-banner eshell-modules-list)
  ;; use TRAMP sudo method to avoid retyping sudo password on multiple calls:
  (push 'eshell-tramp eshell-modules-list))

(use-package ess-site
  ;; ESS (Emacs Speaks Statistics) is a great project that makes Emacs
  ;; speak with R and other statistical languages
  :demand t
  :load-path "lib/ess/lisp"
  :bind
  (:map ess-mode-map
        ("M-=" . ess-insert-S-assign)
        ("_"   . self-insert-command)
        ("M-p" . my/add-pipe)
        ("C-|" . my/ess-eval-pipe-through-line)
        :map inferior-ess-mode-map
        ("M-=" . ess-insert-S-assign)
        ("_"   . self-insert-command))
  :custom
  (ess-ask-for-ess-directory nil "Don't ask for dir when starting a procecss")
  (ess-default-style 'RStudio)
  (ess-eldoc-show-on-symbol t "Show eldoc on symbol instead of only inside of parens")
  (ess-eval-visibly 'nowait "Don't hog Emacs")
  (ess-history-directory (concat user-emacs-directory "var/Rhist/") "Save R history in one place rather than making .Rhistory files everywhere.")
  (ess-pdf-viewer-pref "emacsclient")
  (ess-use-ido nil "I prefer helm.")
  (ess-nuke-trailing-whitespace-p t)
  (ess-R-font-lock-keywords
   '((ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:constants . t)
     (ess-fl-keyword:fun-calls . nil)
     (ess-fl-keyword:numbers . t)
     (ess-fl-keyword:operators . t)
     (ess-fl-keyword:delimiters . nil)
     (ess-fl-keyword:= . t)
     (ess-R-fl-keyword:F&T . t)))
  (inferior-R-font-lock-keywords
   '((ess-S-fl-keyword:prompt . t)
     (ess-R-fl-keyword:messages . t)
     (ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:constants . t)
     (ess-fl-keyword:matrix-labels . t)
     (ess-fl-keyword:fun-calls . nil)
     (ess-fl-keyword:numbers . nil)
     (ess-fl-keyword:operators . t)
     (ess-fl-keyword:delimiters . nil)
     (ess-fl-keyword:= . t)
     (ess-R-fl-keyword:F&T . t)))
  :config
  ;; Make that folder if needed.
  (mkdir ess-history-directory t)
  (defun my/add-pipe ()
    "Adds a pipe operator %>% with one space to the left and then
starts a newline with proper indentation"
    (interactive)
    (just-one-space 1)
    (insert "%>%")
    (ess-newline-and-indent))
  ;; I sometimes want to evaluate just part of a piped sequence. The
  ;; following lets me do so without needing to insert blank lines or
  ;; something:
  (defun my/ess-beginning-of-pipe-or-end-of-line ()
    "Find point position of end of line or beginning of pipe %>%"
    (if (search-forward "%>%" (line-end-position) t)
        (let ((pos (progn
                     (beginning-of-line)
                     (search-forward "%>%" (line-end-position))
                     (backward-char 3)
                     (point))))
          (goto-char pos))
      (end-of-line)))

  (defun my/ess-eval-pipe-through-line (vis)
    "Like `ess-eval-paragraph' but only evaluates up to the pipe on this line.

If no pipe, evaluate paragraph through the end of current line.

Prefix arg VIS toggles visibility of ess-code as for `ess-eval-region'."
    (interactive "P")
    (save-excursion
      (let ((end (progn
                   (my/ess-beginning-of-pipe-or-end-of-line)
                   (point)))
            (beg (progn (backward-paragraph)
                        (ess-skip-blanks-forward 'multiline)
                        (point))))
        (ess-eval-region beg end vis)))))

(use-package exec-path-from-shell
  ;; This ensures Emacs has the same PATH as the rest of my system.  It is
  ;; necessary for macs (not that I ever use that), or if Emacs is started
  ;; via a systemd service, as systemd user services don't inherit the
  ;; environment of that user
  :if (or (eq system-type 'darwin)
          (and (daemonp)
               (eq system-type 'gnu/linux)))
  :config
  (exec-path-from-shell-initialize))

(use-package executable
  :hook
  ;; Emacs can set file permissions automatically.  Make scripts executable
  ;; so I don't have to remember to do so:
  (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package eww
  :commands (eww eww-search-words)
  :bind
  ;; If a webpage requires more than eww can handle, I can switch to the
  ;; system default by tapping &, but 0 is easier to type:
  (:map eww-mode-map
        ("0" . eww-browse-with-external-browser)))

(use-package faces
  ;; faces are how Emacs determines how to display characters (font, size,
  ;; color, etc)
  :defer t
  :bind
  ("C-h c" . describe-face) ; overrides describe-key-briefly from help.el
  :custom-face
  (fixed-pitch-serif ((t (:font "Symbola"))))
  :config
  (add-to-list 'default-frame-alist
               '(font . "monospace-12")))

(use-package face-remap
  :bind
  ;; Everywhere else you can zoom with C-- and C-+.  Let's make Emacs
  ;; follow that convention:
  ("C-+" . text-scale-increase)
  ("C--" . text-scale-decrease))

(progn ; `files.el'
  ;; Don't ask me when I try to create a new file.  Just create it.
  (setq confirm-nonexistent-file-or-buffer nil)
  ;; make final newlines if they don't exist:
  (setq require-final-newline t)

  ;; C-x C-c is originally bound to kill emacs. I accidentally type this
  ;; from time to time which is super-frustrating.  Get rid of it:
  (unbind-key "C-x C-c")

  ;; C-x C-s saves the current buffer and C-x s saves all modified buffers
  ;; (visiting files), but asks about each one.  Let's create a binding to
  ;; save all file visiting buffers without asking:
  (defun my/save-everything-noconfirm ()
    "Calls `save-some-buffers' but doesn't confirm about saving."
    (interactive)
    (save-some-buffers t))
  (bind-key "s-s" #'my/save-everything-noconfirm)


  ;; This lets me make directories on the fly similar to mkdir -p. Thanks
  ;; --- http://mbork.pl/2016-07-25_Making_directories_on_the_fly
  (defun make-parent-directory ()
    "Make sure the directory of `buffer-file-name' exists."
    (make-directory (file-name-directory buffer-file-name) t))
  (add-hook 'find-file-not-found-functions #'make-parent-directory)
  ;; It's nice to have a function to find this file quickly. Here's one:
  (defun my/find-emacs-file ()
    "Find my emacs org file"
    (interactive)
    (find-file (concat user-emacs-directory "init.el")))
  (bind-key "e" #'my/find-emacs-file my/map)
  ;; Sometimes stuff gets out of whack, this helps me put it back in whack:
  (defun my/save-and-revert-buffer ()
    "Save and then revert this buffer."
    (interactive)
    (progn
      (save-buffer)
      (revert-buffer :noconfirm t)))
  (bind-key "G" #'my/save-and-revert-buffer my/map)
  ;; Prompt me to save changed buffers if I'm closing the last frame (and
  ;; Emacs is running as a daemon):
  (when (daemonp)
    (add-to-list 'delete-frame-functions
                 (defun my/save-if-last-frame (frame)
                   (when (eq 1 (cl-count-if
                                (lambda (f)
                                  (eq
                                   (frame-parameter f 'display)
                                   (frame-parameter frame 'display)))
                                (visible-frame-list)))
                     (save-some-buffers))))))

(use-package flymake
  :defer t
  :custom
  (flymake-proc-compilation-prevents-syntax-check nil)
  :hook
  ;; Some modes turn `flymake-mode' on by default, I have to tell these
  ;; specifically to do it:
  ((emacs-lisp-mode python-mode LaTeX-mode). flymake-mode)
  :bind
  (:map flymake-mode-map
        ("M-P" . flymake-goto-prev-error)
        ("M-N" . flymake-goto-next-error)))

(use-package flyspell
  ;; on the fly spell checking
  :custom
  (flyspell-abbrev-p t)
  (flyspell-issue-welcome-flag nil)
  (flyspell-use-global-abbrev-table-p t)
  (flyspell-use-meta-tab nil)
  :hook
  (text-mode . turn-on-flyspell)
  ((prog-mode ess-mode) . flyspell-prog-mode))

(use-package frame
  :defer t
  :config
  ;; don't bind C-x C-z to suspend-frame:
  (unbind-key "C-x C-z")
  ;; In fact, I find suspend-frame so unhelpful let's disable it:
  (put 'suspend-frame 'disabled t)
  ;; A blinking cursor gets kinda annoying, so get rid of it:
  (blink-cursor-mode -1))

(use-package git-timemachine
  ;; And to step through the history of a file:
  :bind
  ("C-x M-g t" . git-timemachine))

(use-package gitattributes-mode
  :defer t)
(use-package gitconfig-mode
  :defer t)
(use-package gitignore-mode
  :defer t)

;;; Helm
(use-package helm
  ;; A package in a league of its own: https://tuhdo.github.io/helm-intro.html
  ;; load it soon after starting Emacs:
  :defer 1
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("M-y" . helm-show-kill-ring)
   ("C-M-z" . helm-resume)
   ([remap occur] . helm-occur)
   ([remap bookmark-jump] . helm-bookmarks)
   ("C-x b" . helm-buffers-list)
   ("C-x C-b" . helm-buffers-list)
   ("M-s M-g" . helm-google-suggest)
   ("M-o" . helm-semantic-or-imenu)
   ("C-h SPC" . helm-all-mark-rings)
   ("M-s g" . helm-grep-do-git-grep)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-i" . helm-execute-persistent-action)
   ("C-z" . helm-select-action))
  :custom
  (helm-display-header-line nil)
  (helm-echo-input-in-header-line t)
  (helm-net-prefer-curl t)
  (helm-split-window-default-side 'below)
  (helm-split-window-inside-p t)
  (helm-command-prefix-key "M-,")
  :init
  (require 'helm-config)
  :config
  (use-package helm-files
    :config
    (push ".git$" helm-boring-file-regexp-list))
  (use-package helm-org
    :bind
    (:map my/map
          ("t" . helm-org-agenda-files-headings)))
  (helm-mode))

(use-package helm-bibtex
  ;; Helm and references
  :bind
  (:map my/map
        ("r b" . my/find-bib-file))
  :custom
  (bibtex-completion-bibliography "~/Sync/bibliography/references.bib")
  (bibtex-completion-cite-default-command 'autocite)
  (bibtex-completion-library-path "~/Sync/bibliography/bibtex-pdfs")
  (bibtex-completion-notes-extension ".org")
  (bibtex-completion-notes-path "~/Sync/bibliography/notes")
  (bibtex-completion-notes-template-multiple-files "* TODO ${year} - ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :AUTHOR: ${author}\n  :JOURNAL: ${journal}\n  :YEAR: ${year}\n  :VOLUME: ${volume}\n  :PAGES: ${pages}\n  :DOI: ${doi}\n  :URL: ${url}\n :END:\n")
  (bibtex-completion-cite-commands '("autocite" "textcite" "citep" "citet" "citeauthor" "citeyear" "Citep" "Citet"))
  :config
  (defun my/find-bib-file ()
    "Find my main bib file."
    (interactive)
    (find-file bibtex-completion-bibliography)))

(use-package helm-c-yasnippet
  ;; I can use this when I can't remember the exact name of a snippet.
  :after yasnippet
  :bind
  ("M-`" . helm-yas-complete))

(use-package helm-mu
  :defer t
  :after (helm mu4e)
  :bind
  (("M-s m" . helm-mu)
   ("M-s c" . helm-mu-contacts)
   :map mu4e-main-mode-map
   ("s" . helm-mu)
   :map mu4e-headers-mode-map
   ("s" . helm-mu)
   :map mu4e-view-mode-map
   ("s" . helm-mu)))

(use-package helm-pass
  ;; Set up helm to easily find passwords, this relies on having pass set
  ;; up (with password-store package)
  :if (executable-find "pass")
  :bind ("M-s p" . helm-pass))

(use-package helm-projectile
  :after (helm projectile)
  :config
  (helm-projectile-on))

(use-package help
  ;; Emacs has an amazing help system built in. C-h v, C-h f, and C-h k are
  ;; bound to describe-variable, describe-function, and describe-key
  ;; respectively.
  :bind
  (:map help-mode-map
        ;; shortcuts for searching from *Help* buffers
        ("v" . describe-variable)
        ("f" . describe-function)
        ("k" . describe-key))
  :custom
  ;; This makes emacs switch focus to help windows:
  (help-window-select t))

(use-package highlight-numbers
  ;; I like to see numbers in code:
  :commands (highlight-numbers-mode)
  :hook
  ((prog-mode) . highlight-numbers-mode))

(use-package hippie-exp
  :bind
  (("M-SPC" . hippie-expand)
   ([remap dabbrev-expand] . hippie-expand))
  :custom
  (hippie-expand-try-functions-list
   '(;; try yasnippet:
     yas-hippie-try-expand
     ;; Try to expand word "dynamically", searching the current buffer.
     try-expand-dabbrev
     ;; Try to expand word "dynamically", searching all other buffers.
     try-expand-dabbrev-all-buffers
     ;; Try to complete text as a file name, as many characters as unique.
     try-complete-file-name-partially
     ;; Try to complete text as a file name.
     try-complete-file-name
     ;; Try to expand word "dynamically", searching the kill ring.
     try-expand-dabbrev-from-kill
     ;; Try to complete the current line to an entire line in the buffer.
     try-expand-list
     ;; Try to complete the current line to an entire line in the buffer.
     try-expand-line))
  (hippie-expand-verbose nil))

(use-package holidays
  :defer t
  :custom
  ;; Emacs knows about holidays, but there are lots that don't affect me.
  ;; Let's hide them
  (holiday-bahai-holidays nil)
  (holiday-hebrew-holidays nil)
  (holiday-islamic-holidays nil)
  (holiday-oriental-holidays nil)
  (holiday-christian-holidays nil))

(use-package hydra
  ;; Hydra is a nice package that lets you set up menus for related (or not)
  ;; commands.
  :bind
  ("C-v" . my/scrolling-up/body)
  ("M-v" . my/scrolling-down/body)
  :config
  ;; the defaults C-v and M-v scroll a full page, which is too much.
  ;; rebind to a half page:
  (defun my/scroll-down ()
    "Scroll a half page up."
    (interactive)
    (let ((count (/ (1- (window-height)) 2)))
      (progn
        (scroll-down count))))
  (defun my/scroll-up ()
    "Scroll a half page up."
    (interactive)
    (let ((count (/ (1- (window-height)) 2)))
      (progn
        (scroll-up count))))
  (defhydra my/scrolling-down (:body-pre my/scroll-down)
    "Scroll without needing to hold C"
    ("v" my/scroll-up "up")
    ("C-v" my/scroll-up "up")
    ("M-v" my/scroll-down "down"))
  (defhydra my/scrolling-up (:body-pre my/scroll-up)
    "Scroll without needing to hold C"
    ("v" my/scroll-up "up")
    ("C-v" my/scroll-up "up")
    ("M-v" my/scroll-down "down")))

(progn ; `isearch'
  ;; isearch is the package that provides Emacs's forward and reverse
  ;; searching.  These are bound to =C-s= and =C-r= by default.  If you've
  ;; already started a search with =C-s=, then backspace sometimes doesn't
  ;; delete characters; it goes back to the previous match.  I prefer
  ;; backspace to always delete characters; I can just =C-r= to get to the
  ;; previous match.
  (bind-key "<backspace>" #'isearch-del-char isearch-mode-map)
  ;; Use regex searches by default:
  (setq search-default-mode t))

(use-package ledger-mode
  ;; ledger is a program that I use to keep track of finances. Emacs, of course,
  ;; can handle it quite nicely.
  :if (executable-find "ledger")
  :hook
  (ledger-mode . my/setup-ledger-mode)
  (ledger-mode . ledger-flymake-enable)
  :bind
  (:map ledger-mode-map
        ("C-c r" . ledger-reconcile)
        :map my/map
        ("l" . my/ledger-file))
  :custom
  (ledger-default-date-format "%Y-%m-%d" "ISO dates.")
  (ledger-flymake-be-explicit t "Warn about account typos.")
  (ledger-flymake-be-pedantic t "Warn about account typos.")
  (ledger-post-amount-alignment-at :decimal "Align at decimal place.")
  (ledger-post-amount-alignment-column 70 "Align at column 70")
  (ledger-report-resize-window nil "Don't resize windows.")
  (ledger-report-use-header-line t "Write info in the `header-line', leaving buffer for reports.")
  (ledger-mode-should-check-version nil "Assume ledger is up-to-date.")
  (ledger-reports
   '(("on-hand"             "%(binary) -f %(ledger-file) bal assets liabilities -X $ --current")
     ("account"             "%(binary) -f %(ledger-file) reg %(account)")
     ("expenses (monthly)"  "%(binary) -f %(ledger-file) reg expenses -X $ -M ")
     ("expenses (yearly)"   "%(binary) -f %(ledger-file) reg expenses -X $ -Y ")
     ("cash-flow-monthly"   "%(binary) -f %(ledger-file) -X $ --invert -b \"this month\" bal income expenses")
     ("cash-flow-YTD"       "%(binary) -f %(ledger-file) -X $ --invert -b \"this year\"  bal income expenses")
     ("budget (this month)" "%(binary) -f %(ledger-file) budget ^exp -X $ -b \"this month\" --flat")
     ("budget (YTD)"        "%(binary) -f %(ledger-file) budget ^exp -X $ -b \"this year\" --flat")))
  :init
  (defvar my/ledger-file
    (expand-file-name "~/Sync/Finances/finances.ledger")
    "Where the ledger journal is kept.")
  (defun my/ledger-file ()
    "Find ledger journal file."
    (interactive)
    (find-file my/ledger-file))
  :config
  (setq ledger-account-name-or-directive-regex ledger-account-directive-regex)
  (defun my/setup-ledger-mode ()
    "Setup `ledger-mode' how I like."
    ;; disable company mode in ledger mode because ledger-mode comes
    ;; with a great completion engine (magic TAB):
    (company-mode -1)))

(use-package magit
  ;; magit is magical git
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g b" . magit-blame)
  :custom
  (magit-log-section-commit-count 0 "Don't show recent commits in magit-status.")
  (magit-diff-refine-hunk 'all "Get highlighted word diffs.")
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package markdown-mode
  ;; Markdown mode for Markdown editing!
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.Rmd\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind
  (:map markdown-mode-map
        ("M-p" . markdown-previous-visible-heading)
        ("M-n" . markdown-next-visible-heading))
  :custom
  (markdown-enable-math t))

(use-package minibuffer
  :defer t
  :custom
  (read-file-name-completion-ignore-case t "Ignore file case when trying to find stuff:"))

(use-package minions
  ;; Set up how minor modes are displayed in the mode-line
  :custom
  (minions-direct '(flymake-mode) "I always want flymake-mode visible because it shows counts of errors/warnings.")
  (minions-mode-line-lighter "…" "Don't wink at me.")
  :hook
  (after-init . minions-mode))

(use-package mixed-pitch
  ;; Emacs was an editor originally designed for code, so it defaults to a
  ;; fixed-width font for most things.  It's perfectly capable of handling
  ;; variable-pitch fonts, though.  The built-in command
  ;; variable-pitch-mode can do that for any buffer.  However, I oftentimes
  ;; work in a buffer that mixes things that I'd prefer to be fixed-width
  ;; (e.g. code) and variable width (text).  You can convince Emacs to use
  ;; fixed-width for some faces and variable-width fonts for others, but
  ;; you have to specify what faces should be what.  Luckily,
  ;; [[https://ogbe.net/emacsconfig.html][someone]] has already figured
  ;; this out.  I took their code, modified it to work more like what I
  ;; want, and packaged it up.
  :bind
  (:map my/map
        ("f" . mixed-pitch-mode))
  :config
  (set-face-attribute 'variable-pitch nil :family "Linux Libertine" :height 160))

(use-package moody
  ;; Set up the `mode-line'
  :demand
  :custom
  (moody-mode-line-height (if (string= (system-name) "mars") 22 40))
  (x-underline-at-descent-line t)
  :custom-face
  (mode-line ((t (:box nil :background "#5d4d7a" :underline "#5d4d7a" :overline "#5d4d7a"))))
  (mode-line-inactive ((t (:box nil :underline "#5d4d7a" :overline "#5d4d7a"))))
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package mouse
  :defer t
  :bind
  ;; We can use shift-mouse for selecting from point:
  ("<S-down-mouse-1>" . mouse-save-then-kill)
  :custom
  (mouse-yank-at-point t ))

(use-package mu4e
  :if (executable-find "mu")
  :defer 10
  :commands (mu4e)
  :hook
  ;; Wrap lines at `fill-column' when viewing emails:
  (mu4e-compose-mode . autofill-off-visual-on)
  (mu4e-headers-mode . my/setup-mu4e-headers)
  :bind
  (("C-x m" . mu4e-compose-new)
   ("<f9>" . my/work-inbox)
   ("<f10>" . my/personal-inbox)
   :map mu4e-headers-mode-map
   ("d" . mu4e-headers-mark-for-delete)
   ("q" . mu4e-quit-session)
   ("c" . my/org-mu4e-store-and-capture)
   :map mu4e-view-mode-map
   ("d" . mu4e-view-mark-for-delete)
   ("<tab>" . shr-next-link)
   ("<backtab>" . shr-previous-link)
   ("c" . my/org-mu4e-store-and-capture)
   :map mu4e-main-mode-map
   ("q" . mu4e-quit-session)
   :map my/map
   ("m" . my-mu4e-start))
  :custom
  (mu4e-maildir "~/.mail")
  (mu4e-context-policy 'pick-first)
  (mu4e-maildir-shortcuts '(("/utexas/INBOX"  . ?u)
                            ("/gmail/INBOX" . ?g)))
  (mu4e-save-multiple-attachments-without-asking t "Save all attachments in same dir.")
  (mu4e-confirm-quit nil "Don't ask me to quit, just quit.")
  (mail-user-agent 'mu4e-user-agent)
  (mu4e-sent-messages-behavior 'delete "Don't save message to Sent Messages, Gmail/IMAP takes care of this.")
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-update-interval 80 "In seconds.")
  (mu4e-change-filenames-when-moving t "For mbsync")
  (mu4e-user-mail-address-list '("branham@utexas.edu"
                                 "alex.branham@gmail.com"
                                 "james.alexander.branham@gu.se"))
  (mu4e-compose-dont-reply-to-self t "Don't reply to self.")
  (mu4e-compose-complete-only-personal t)
  (mu4e-compose-complete-only-after "2015-01-01")
  (mu4e-view-show-addresses t)
  (mu4e-hide-index-messages t)
  (mu4e-view-show-images t)
  (message-kill-buffer-on-exit t)
  (mu4e-use-fancy-chars t)
  (mu4e-headers-skip-duplicates t)
  (mu4e-headers-include-related nil "Don't include related messages, as threads can be quite long.")
  (mu4e-attachment-dir "~/Downloads")
  (mu4e-completing-read-function 'completing-read)
  (mu4e-headers-date-format "%F" "ISO dates.")
  (mu4e-headers-fields '((:human-date    .  11)
                         (:flags         .   6)
                         (:mailing-list   .  10)
                         (:from-or-to    .  22)
                         (:subject       .  nil)))
  ;; next two are from:
  ;; http://pragmaticemacs.com/emacs/customise-the-reply-quote-string-in-mu4e/ :
  ;; customize the reply-quote-string
  (message-citation-line-format "On %a %d %b %Y at %R, %f wrote:\n")
  ;; choose to use the formatted string
  (message-citation-line-function #'message-insert-formatted-citation-line)
  :config
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
  (defun my/work-inbox ()
    "Jump to work email"
    (interactive)
    (window-configuration-to-register :mu4e-fullscreen)
    (mu4e~headers-jump-to-maildir "/utexas/INBOX")
    (delete-other-windows))
  (defun my/personal-inbox ()
    "Jump to personal email"
    (interactive)
    (window-configuration-to-register :mu4e-fullscreen)
    (mu4e~headers-jump-to-maildir "/gmail/INBOX")
    (delete-other-windows))
  (defun my/setup-mu4e-headers ()
    "Set up mu4e headers."
    ;; Don't wrap long lines, let me scroll a single line horizontally
    (visual-line-mode -1)
    (setq-local auto-hscroll-mode 'current-line)
    (toggle-truncate-lines 1))
  (defun my/org-mu4e-store-and-capture ()
    "Similar to `org-mu4e-store-and-capture', but use \"r\" capture template and then mark the email for deletion."
    (interactive)
    (org-mu4e-store-link)
    (org-capture :keys "r")
    (if (eq major-mode 'mu4e-headers-mode)
        (mu4e-headers-mark-for-delete)
      (mu4e-view-mark-for-delete)))
  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "utexas"
             :enter-func (lambda () (mu4e-message "Switch to utexas context"))
             ;; leave-func not defined
             :match-func (lambda (msg)
                           (when msg
                             (string-prefix-p "/utexas" (mu4e-message-field msg :maildir))))
             :vars '((mu4e-drafts-folder           . "/utexas/[Gmail]/.Drafts")
                     (mu4e-sent-folder             . "/utexas/[Gmail]/.Sent Mail")
                     (mu4e-trash-folder            . "/utexas/[Gmail]/.Trash")
                     (user-mail-address            . "branham@utexas.edu")
                     (user-full-name               . "Alex Branham")
                     (smtpmail-smtp-user           . "branham@utexas.edu")
                     (smtpmail-default-smtp-server . "smtp.gmail.com")
                     (smtpmail-smtp-server         . "smtp.gmail.com")
                     (smtpmail-smtp-service        . 587)
                     (smtpmail-stream-type         . starttls)
                     (mu4e-compose-signature-auto-include . t)
                     (mu4e-compose-signature       . (concat
                                                      "J. Alexander Branham\n"
                                                      "PhD Candidate\n"
                                                      "Department of Government\n"
                                                      "University of Texas at Austin\n"
                                                      "https://www.jabranham.com"
                                                      "\n"))))
           ,(make-mu4e-context
             :name "gu"
             :enter-func (lambda () (mu4e-message "Switch to gu context"))
             ;; leave-func not defined
             :match-func (lambda (msg)
                           (when msg
                             (string-prefix-p "/gu" (mu4e-message-field msg :maildir))))
             :vars '((mu4e-drafts-folder           . "/gu/Drafts")
                     (mu4e-sent-folder             . "/gu/Sent")
                     (mu4e-refile-folder           . "/gu/Archive")
                     (mu4e-trash-folder            . "/gu/Trash")
                     (user-mail-address            . "james.alexander.branham@gu.se")
                     (user-full-name               . "Alex Branham")
                     (smtpmail-smtp-user           . "james.alexander.branham@gu.se")
                     (smtpmail-smtp-server         . "localhost"); using davmail to access the Exchange server
                     (smtpmail-default-smtp-server . "localhost")
                     (smtpmail-stream-type         . plain)
                     (smtpmail-smtp-service        . 1025)
                     (mu4e-compose-signature-auto-include . t)
                     (mu4e-compose-signature       . (concat
                                                      "J. Alexander Branham\n"
                                                      "Research Associate\n"
                                                      "Department of Political Science\n"
                                                      "Göteborgs Universitet\n"
                                                      "https://www.jabranham.com"
                                                      "\n"))))
           ,(make-mu4e-context
             :name "personal"
             :enter-func (lambda () (mu4e-message "Switch to personal context"))
             ;; leave-func not defined
             :match-func (lambda (msg)
                           (when msg
                             (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
             :vars '((user-mail-address            . "alex.branham@gmail.com")
                     (user-full-name               . "Alex Branham")
                     (smtpmail-smtp-user           . "alex.branham@gmail.com")
                     (smtpmail-default-smtp-server . "smtp.gmail.com")
                     (smtpmail-smtp-server         . "smtp.gmail.com")
                     (smtpmail-smtp-service        . 587)
                     (smtpmail-stream-type         . starttls)
                     (mu4e-compose-signature-auto-include . nil)
                     (mu4e-drafts-folder           . "/gmail/[Gmail]/.Drafts")
                     (mu4e-sent-folder             . "/gmail/[Gmail]/.Sent Mail")
                     (mu4e-trash-folder            . "/gmail/[Gmail]/.Trash")))))
  ;; turn off autofill mode in mu4e compose
  (defun autofill-off-visual-on ()
    "Turn off auto-fill-mode and turn on visual-mode"
    (auto-fill-mode -1)
    (visual-line-mode))
  ;; use aV to open message in browser
  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  (use-package gnus-dired
    :hook
    (dired-mode . turn-on-gnus-dired-mode)
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
    (setq gnus-dired-mail-mode 'mu4e-user-agent))
  ;; configure orgmode support in mu4e
  (use-package org-mu4e
    ;; when mail is sent, automatically convert org body to HTML
    :config
    (setq org-mu4e-convert-to-html t))
  ;; Here we define a function that cleans up contacts. Stolen from:
  ;; https://martinralbrecht.wordpress.com/2016/05/30/handling-email-with-emacs/
  (defun malb/canonicalise-contact-name (name)
    (let ((case-fold-search nil))
      (setq name (or name ""))
      (if (string-match-p "^[^ ]+@[^ ]+\.[^ ]" name)
          ""
        (progn
          ;; drop email address
          (setq name
                (replace-regexp-in-string
                 "^\\(.*\\) [^ ]+@[^ ]+\.[^ ]" "\\1" name))
          ;; strip quotes
          (setq name
                (replace-regexp-in-string "^\"\\(.*\\)\"" "\\1" name))
          ;; deal with YELL’d last names
          (setq name
                (replace-regexp-in-string
                 "^\\(\\<[[:upper:]]+\\>\\) \\(.*\\)" "\\2 \\1" name))
          ;; Foo, Bar becomes Bar Foo
          (setq name
                (replace-regexp-in-string
                 "^\\(.*\\), \\([^ ]+\\).*" "\\2 \\1" name))))))
  (defun malb/mu4e-contact-rewrite-function (contact)
    (let* ((name (or (plist-get contact :name) ""))
           (case-fold-search nil))
      (plist-put contact :name (malb/canonicalise-contact-name name))
      contact))
  (setq mu4e-contact-rewrite-function #'malb/mu4e-contact-rewrite-function)

  ;; function to return first name of email recipients
  ;; used by yasnippet
  ;; inspired by
  ;;http://blog.binchen.org/posts/how-to-use-yasnippets-to-produce-email-templates-in-emacs.html
  (defun bjm/mu4e-get-names-for-yasnippet ()
    "Return comma separated string of names for an email"
    (interactive)
    (let ((email-name "") str email-string email-list email-name2 tmpname)
      (save-excursion
        (goto-char (point-min))
        ;; first line in email could be some hidden line containing NO to
        ;; field
        (setq str (buffer-substring-no-properties (point-min) (point-max))))
      ;; take name from TO field - match series of names
      (when (string-match "^To: \"?\\(.+\\)" str)
        (setq email-string (match-string 1 str)))
      ;; split to list by comma
      (setq email-list (split-string email-string " *, *"))
      ;; loop over emails
      (dolist (tmpstr email-list)
        ;;get first word of email string
        (setq tmpname (car (split-string tmpstr " ")))
        ;;remove whitespace or ""
        (setq tmpname (replace-regexp-in-string "[ \"]" "" tmpname))
        ;;join to string
        (setq email-name
              (concat email-name ", " tmpname)))
      ;; remove initial comma
      (setq email-name (replace-regexp-in-string "^, " "" email-name))
      ;; see if we want to use the name in the FROM field get name in FROM
      ;; field if available, but only if there is only one name in TO field
      (if (< (length email-list) 2)
          (when (string-match "^On.+, \\([^ ,\n]+\\).+wrote:$" str)
            (progn
              (setq email-name2 (match-string 1 str))
              ;;prefer name in FROM field if TO field has "@"
              (when (string-match "@" email-name)
                (setq email-name email-name2))
              )))
      email-name))

  ;; starts mu4e silently when emacs starts
  (mu4e t))

(use-package mu4e-alert
  :if (executable-find "mu")
  :defer 10
  :after mu4e
  :custom
  (mu4e-alert-email-notification-types '(subjects))
  (mu4e-alert-set-window-urgency nil)
  (mu4e-alert-interesting-mail-query (concat
                                      "flag:unread AND maildir:\"/utexas/INBOX\""
                                      " OR flag:unread AND maildir:\"/gmail/INBOX\""
                                      " OR flag:unread AND maildir:\"/gu/INBOX\""))
  :config
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-enable-notifications))

(use-package multiple-cursors
  ;; Emacs can support multiple cursors.  I don't use this much, but it's
  ;; super handy when I do need it:
  :bind ("C-c m c" . my/mc-hydra/body)
  :commands (mc/edit-lines mc/mark-all-like-this)
  :config
  (defhydra my/mc-hydra (:hint nil)
    "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("r" mc/mark-all-in-region-regexp :exit t)
    ("q" nil)))

(use-package mwheel
  :defer t
  :custom
  (mouse-wheel-scroll-amount '(1 ((shift) . 1)) "One line at a time.")
  (mouse-wheel-progressive-speed nil "Don't accelerate scrolling.")
  (mouse-wheel-follow-mouse 't "Scroll window under mouse."))

(use-package ob-core
  ;; ob is org-babel, which lets org know about code and code blocks
  :defer t
  :custom
  ;; I know what I'm getting myself into.
  (org-confirm-babel-evaluate nil "Don't ask to confirm evaluation."))

(use-package org
  ;; Org mode is a great thing. I use it for writing academic papers,
  ;; managing my schedule, managing my references and notes, writing
  ;; presentations, writing lecture slides, and pretty much anything
  ;; else.
  :bind
  (("C-c l" . org-store-link)
   ("C-'" . org-cycle-agenda-files) ; quickly access agenda files
   :map org-mode-map
   ;; I rebind C-c C-r to look at my reference list globally (see org-ref
   ;; below), which overrides the default binding of org-reveal.  However,
   ;; that command is quite useful, so let's bind it to C-c r:
   ("C-c r" . org-reveal)
   ;; Bind M-p and M-n to navigate heading more easily (these are bound to
   ;; C-c C-p/n by default):
   ("M-p" . org-previous-visible-heading)
   ("M-n" . org-next-visible-heading)
   ;; C-c C-t is bound to `org-todo' by default, but I want it
   ;; bound to C-c t as well:
   ("C-c t" . org-todo))
  :custom
  (org-pretty-entities t "UTF8 all the things!")
  (org-support-shift-select t "Holding shift and moving point should select things.")
  (org-fontify-quote-and-verse-blocks t "Provide a special face for quote and verse blocks.")
  (org-M-RET-may-split-line nil "M-RET may never split a line.")
  (org-enforce-todo-dependencies t "Can't finish parent before children.")
  (org-enforce-todo-checkbox-dependencies t "Can't finish parent before children.")
  (org-hide-emphasis-markers t "Make words italic or bold, hide / and *.")
  (org-catch-invisible-edits 'show-and-error "Don't let me edit things I can't see.")
  (org-special-ctrl-a/e t "Make C-a and C-e work more like how I want:.")
  (org-preview-latex-default-process 'imagemagick "Let org's preview mechanism use imagemagick instead of dvipng.")
  (org-imenu-depth 6 "Imenu can go deep into menu structure since I use helm.")
  (org-image-actual-width '(300))
  (org-blank-before-new-entry '((heading . nil)
                                (plain-list-item . nil)))
  ;; For whatever reason, I have to explicitely tell org how to open pdf
  ;; links.  I use pdf-tools.  If pdf-tools isn't installed, it will use
  ;; doc-view (shipped with Emacs) instead.
  (org-file-apps
   '((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . emacs)))
  (org-highlight-latex-and-related '(latex entities) "set up fontlocking for latex")
  (org-startup-with-inline-images t "Show inline images.")
  (org-log-done t)
  (org-goto-interface 'outline-path-completion)
  (org-ellipsis "⬎")
  (org-tag-persistent-alist '(("jobs" . ?j)
                              (:startgroup . nil)
                              ("@work" . ?w)
                              ("@home" . ?h)
                              (:endgroup . nil)))
  ;; I keep my recipes in an org file and tag them based on what kind of
  ;; dish they are.  The level one headings are names, and each gets two
  ;; level two headings --- ingredients and directions.  To easily search via
  ;; tag, I can restrict org-agenda to that buffer using < then hit m to
  ;; match based on a tag.
  (org-tags-exclude-from-inheritance
   '("BREAKFAST" "DINNER" "DESSERT" "SIDE" "CHICKEN" "PORK" "SEAFOOD"
     "BEEF" "PASTA" "SOUP" "SNACK" "DRINK" "LAMB"))
  ;; Org-refile lets me quickly move around headings in org files.  It
  ;; plays nicely with org-capture, which I use to turn emails into TODOs
  ;; easily (among other things, of course)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-use-outline-path t)
  (org-refile-targets '((org-default-notes-file . (:maxlevel . 6))
                        (my/org-inbox . (:maxlevel . 2))
                        (my/org-scheduled . (:level . 1))
                        (my/org-notes . (:maxlevel . 6))))
  :config
  ;; These are the programming languages org should teach itself:
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (python . t)
     (R . t)
     (shell . t)))
  ;; remove C-c [ from adding org file to front of agenda
  (unbind-key "C-c [" org-mode-map))

(use-package org-agenda
  ;; Here's where I set which files are added to org-agenda, which controls
  ;; org's global todo list, scheduling, and agenda features.  I use
  ;; Syncthing to keep these files in sync across computers.
  :bind
  (("C-c a" . org-agenda)
   ("<f5>" . org-agenda)
   :map my/map
   ("a" . my/agenda)
   :map org-agenda-mode-map
   ;; overrides org-agenda-redo, which I use "g" for anyway
   ("r" . org-agenda-refile)
   ;; overrides saving all org buffers, also bound to C-x C-s
   ("s" . org-agenda-schedule)
   ;; overrides org-exit
   ("x" . my/org-agenda-mark-done))
  :custom
  (org-directory "~/org/" "Kept in sync with syncthing.")
  (org-default-notes-file (concat org-directory "todo.org"))
  (org-agenda-skip-deadline-if-done t "Remove done deadlines from agenda.")
  (org-agenda-skip-scheduled-if-done t "Remove done scheduled from agenda.")
  (org-agenda-skip-timestamp-if-done t "Don't show timestamped things in agenda if they're done.")
  (org-agenda-skip-scheduled-if-deadline-is-shown 'not-today "Don't show scheduled if the deadline is visible unless it's also scheduled for today.")
  (org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled "Skip deadline warnings if it is scheduled.")
  (org-deadline-warning-days 3 "warn me 3 days before a deadline")
  (org-agenda-tags-todo-honor-ignore-options t "Ignore scheduled items in tags todo searches.")
  (org-agenda-tags-column 'auto)
  (org-agenda-window-setup 'only-window "Use current window for agenda.")
  (org-agenda-restore-windows-after-quit t "Restore previous config after I'm done.")
  (org-agenda-span 'day) ; just show today. I can "vw" to view the week
  (org-agenda-time-grid
   '((daily today remove-match) (800 1000 1200 1400 1600 1800 2000)
     "" "") "By default, the time grid has a lot of ugly "-----" lines. Remove those.")
  (org-agenda-scheduled-leaders '("" "%2dx ") "I don't need to know that something is scheduled.  That's why it's appearing on the agenda in the first place.")
  (org-agenda-block-separator 8212 "Use nice unicode character instead of ugly = to separate agendas:")
  (org-agenda-deadline-leaders '("Deadline: " "In %d days: " "OVERDUE %d day: ") "Make deadlines, especially overdue ones, stand out more:")
  (org-agenda-current-time-string "⸻ NOW ⸻")
  ;; The agenda is ugly by default. It doesn't properly align items and it
  ;; includes weird punctuation. Fix it:
  (org-agenda-prefix-format '((agenda . "%-12c%-14t%s")
                              (timeline . "  % s")
                              (todo . " %i %-12:c")
                              (tags . " %i %-12:c")
                              (search . " %i %-12:c")))
  (org-agenda-custom-commands
   '((" " "Agenda"
      ((agenda "" nil)
       (tags "REFILE"
             ((org-agenda-overriding-header "Tasks to Refile")
              (org-tags-match-list-sublevels nil)))))
     ("h" "Home Agenda"
      ((agenda "" nil)
       (tags "@home"
             ((org-agenda-overriding-header "Tasks to do at home")
              (org-tags-match-list-sublevels nil)))
       (tags "REFILE"
             ((org-agenda-overriding-header "Tasks to Refile")
              (org-tags-match-list-sublevels nil)))))
     ("w" "Work Agenda"
      ((agenda "" nil)
       (tags "@work"
             ((org-agenda-overriding-header "Tasks to do at work")
              (org-tags-match-list-sublevels nil)))
       (tags "REFILE"
             ((org-agenda-overriding-header "Tasks to Refile")
              (org-tags-match-list-sublevels nil)))))
     ("d" "deadlines"
      ((agenda ""
               ((org-agenda-entry-types '(:deadline))
                (org-agenda-span 'fortnight)
                (org-agenda-time-grid nil)
                (org-deadline-warning-days 0)
                (org-agenda-skip-deadline-if-done nil)))))
     ("b" "bibliography"
      ((tags "CATEGORY=\"bib\""
             ((org-agenda-overriding-header "You've got a lot of reading to do...")))))
     ("u" "unscheduled"
      ((todo  "TODO"
              ((org-agenda-overriding-header "Unscheduled tasks")
               (org-agenda-todo-ignore-with-date t)))))))
  :hook
  (org-agenda-mode . hl-line-mode)
  :init
  (defconst my/org-inbox (concat org-directory "refile.org")
    "Inbox for tasks/todo.")
  (defconst my/org-notes (concat org-directory "notes.org")
    "Long-term storage for notes.")
  (defconst my/org-scheduled (concat org-directory "scheduled.org")
    "Scheduled tasks.")
  ;; set up org agenda files for the agenda
  (setq org-agenda-files `(,org-default-notes-file
                           ,my/org-inbox
                           ,my/org-scheduled))
  (setq org-agenda-text-search-extra-files `(,my/org-notes))
  :config
  (defun my/org-agenda-mark-done (&optional _arg)
    "Mark current TODO as DONE.
See `org-agenda-todo' for more details."
    (interactive "P")
    (org-agenda-todo "DONE"))
  (defun my/agenda (&optional arg)
    (interactive)
    (org-agenda arg " ")))

(use-package org-bullets
  ;; UTF-8 bullets for org headings
  :hook
  (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("•") "Use unicode bullets instead of *"))

(use-package org-capture
  ;; I use org-capture to create short notes about all kinds of things.  I
  ;; can capture emails to remember for later, quick thoughts for later,
  ;; RSS feeds, really anything.
  :bind*
  ("C-c c" . org-capture)
  :bind
  (:map org-capture-mode-map
        ("C-c C-j" . my/org-capture-refile-and-jump))
  :custom
  ;; And now for the capture templates themselves.  It's a bit complicated,
  ;; but the manual does a great job explaining.
  (org-capture-templates
   `(("s" "store" entry (file ,my/org-inbox)
      "* TODO %?\n %a \n %i")
     ("t" "task" entry (file  ,my/org-inbox)
      "* TODO %? \n %i")
     ("n" "note" entry (file ,my/org-notes)
      "* %?\n %i")
     ("b" "bib" entry (file+headline ,org-default-notes-file "Bibliography")
      "* TODO %a            :@work:\n \n %i")
     ("r" "refile+schedule" entry (file ,my/org-inbox)
      "* TODO %a %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d 9am\") t)"
      :immediate-finish t))))

(use-package org-eww
  ;; Org-eww lets me capture eww webpages with org-mode
  :after eww)

(use-package org-indent
  ;; org-indent-mode nicely aligns text with the outline level
  :hook
  (org-mode . org-indent-mode))

(use-package org-ref
  :defer 15
  ;; I use org-ref to manage my references.
  :bind*
  (("C-c C-r" . org-ref-helm-insert-cite-link)
   :map bibtex-mode-map
   ("C-c C-c" . org-ref-clean-bibtex-entry)
   :map my/map
   ("r d" . doi-add-bibtex-entry)
   ("r i" . isbn-to-bibtex))
  :custom
  (org-ref-completion-library 'org-ref-helm-bibtex)
  (org-ref-notes-function #'org-ref-notes-function-many-files)
  (org-ref-notes-directory "~/Sync/bibliography/notes")
  (org-ref-default-bibliography '("~/Sync/bibliography/references.bib"))
  (org-ref-pdf-directory  "~/Sync/bibliography/bibtex-pdfs")
  (org-ref-default-ref-type "autoref")
  (org-ref-default-citation-link "autocite")
  ;; Set this to nil; it slows down org a LOT. Agenda generation takes under a
  ;; second when it is nil and over 7 seconds when t:
  (org-ref-show-broken-links nil)
  :config
  ;; TODO: I can't use use-package's :hook keyword to add these two hooks
  ;; because org-ref-clean-bibtex-entry-hook then gets defined before the
  ;; defcustom, which means the other functions don't get added to it.

  ;; Cleanup nil entries from articles.
  (add-hook 'org-ref-clean-bibtex-entry-hook #'orcb-clean-nil-opinionated)
  ;; Fix journal names in bibtex entries
  (add-hook 'org-ref-clean-bibtex-entry-hook #'my/fix-journal-name)


  (defvar my/notes-template
    "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n")
  (setq org-ref-note-title-format my/notes-template)
  ;; Org-ref-bibtex is a package that helps me manage my bib file(s). I add the
  ;; my/fix-journal-name function to always put in the full name of the journal.
  ;; I also add it to the cleaning hook so that it's taken care of for me more
  ;; or less automatically.
  (defun my/add-to-journal-list (element)
    "Add ELEMENT to `org-ref-bibtex-journal-abbreviations'"
    (push element org-ref-bibtex-journal-abbreviations))

  (mapc #'my/add-to-journal-list
        '(("AJPS" "American Journal of Political Science" "Am Jour Polit Sci")
          ("AJPS" "American Journal of Political Science" "Am J Political Science")
          ("APR" "American Politics Research" "Amer. Pol. Res.")
          ("APSR" "American Political Science Review" "Am Polit Sci Rev")
          ("APSR" "American Political Science Review" "The American Political Science Review")
          ("ARPS" "Annual Review of Political Science" "Ann. Rev. Poli. Sci.")
          ("BJPS" "British Journal of Political Science" "Brit J of Pol Sci")
          ("CP" "Comparative Politics" "Comp. Pol.")
          ("CPS" "Comparative Political Studies" "Comp. Pol. Stud.")
          ("EPSR" "European Political Science Review" "Eur. Pol. Sci. Rev.")
          ("EJPR" "European Journal of Political Research" "Eur. Jour. Pol. Res.")
          ("ES" "Electoral Studies" "Elec. Stud.")
          ("EUP" "European Union Politics" "Eur. Unio. Pol.")
          ("IJPOR" "International Journal of Public Opinion Research" "Intl J Pub Opin Res")
          ("IO" "International Organization" "Intl Org")
          ("JEPOP" "Journal of Elections, Public Opinion, and Policy" "Jour Elec PO and Pol")
          ("JEPP" "Journal of European Public Policy" "Jour. Eur. Pub. Pol.")
          ("JEPS" "Journal of Experimental Political Science" "J Exp Poli Sci")
          ("JOD" "Journal of Democracy" "J of Dem")
          ("JOP" "Journal of Politics" "The Journal of Politics")
          ("JOP" "Journal of Politics" "J of Pol")
          ("jop" "Journal of Politics" "J of Pol")
          ("JoC" "Journal of Communication" "J Communication")
          ("LSQ" "Legislative Studies Quarterly" "Leg. Stud. Quar.")
          ("PA" "Political Analysis" "Pol. Analy.")
          ("PB" "Political Behavior" "Pol Behavior")
          ("PC" "Political Communication" "Pol Comm")
          ("PoP" "Perspectives on Politics" "Perspect. polit.")
          ("POQ" "Public Opinion Quarterly" "Pub. Opin. Quar.")
          ("PP" "Party Politics" "Par Pol")
          ("PRQ" "Political Research Quarterly" "Pol. Res. Quar.")
          ("PSJ" "Policy Studies Journal" "Pol Stu Jour")
          ("PSQ" "Presidential Studies Quarterly" "Pres Stud Quar")
          ("PSRM" "Political Science Research and Methods" "Pol. Sci. Res. Meth.")
          ("QJPS" "Quarterly Journal of Political Science" "Quar. Joun. Poli. Sci.")
          ("R\&P" "Research \\& Politics" "Res. and Pol.")
          ("SPPQ" "State Politics \\& Policy Quarterly" "Stat. Pol. Pol. Quar.")
          ("SS" "Statistical Science" "Stat. Sci.")
          ("WEP" "West European Politics" "West Eur. Pol.")
          ("WP" "World Politics" "Wor Pol")))

  (defun my/fix-journal-name (&optional _key _start _end)
    "Replace journal name in a bibtex entry with the full name.
  The strings are defined in
  `org-ref-bibtex-journal-abbreviations'. The optional arguments
  KEY, START and END allow you to use this with
  `bibtex-map-entries'"
    (interactive)
    (bibtex-beginning-of-entry)
    (when
        (string= "article"
                 (downcase
                  (cdr (assoc "=type=" (bibtex-parse-entry)))))
      (let* ((initial-names (mapcar
                             (lambda (row)
                               (cons  (nth 0 row) (nth 1 row)))
                             org-ref-bibtex-journal-abbreviations))
             (abbrev-names (mapcar
                            (lambda (row)
                              (cons  (nth 2 row) (nth 1 row)))
                            org-ref-bibtex-journal-abbreviations))
             (journal (s-trim (bibtex-autokey-get-field "journal")))
             (bstring (or
                       (cdr (assoc journal initial-names))
                       (cdr (assoc journal abbrev-names)))))
        (when bstring
          (bibtex-set-field "journal" bstring)
          (bibtex-fill-entry)))))
  (use-package doi-utils
    :config
    (setq doi-utils-open-pdf-after-download t))
  (use-package org-ref-isbn
    :custom
    (org-ref-isbn-exclude-fields '("form" "lang" "lccn" "oclcnum")))
  (use-package org-ref-latex))

(use-package org-src
  ;; org source code examples
  :defer t
  :custom
  (org-src-tab-acts-natively t "This will make the tab key act like you want it to inside code blocks.")
  (org-src-window-setup 'current-window "Set up src windows in their current window rather than another one."))

(use-package outline
  :defer t
  :bind
  (:map outline-mode-map
        ("M-p" . outline-previous-visible-heading)
        ("M-n" . outline-next-visible-heading)))

(use-package ox
  ;; ox is org's export engine
  :defer t
  :custom
  (org-export-with-smart-quotes t)
  (org-export-with-toc nil "Don't include a table of contents when exporting.")
  ;; This lets me override all the export variables with a =#+BIND:= statement
  ;; at the beginning of org-mode files for export:
  (org-export-allow-bind-keywords t))

(use-package ox-latex
  ;; org's latex/pdf exporting engine
  :defer t
  :custom
  (org-latex-pdf-process '("latexmk -synctex=1 -xelatex %f"))
  :config
  ;; add support for coloring code output.  Use minted if pygments is
  ;; installed, otherwise fall back to the listings package, which doesn't
  ;; require anything other than latex.
  (if (executable-find "pygments")
      ;; use minted
      (progn
        (setq org-latex-listings 'minted)
        (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))
        ;; also need to figure out how to add -shell-escape option to `org-latex-pdf-process'
        )
    ;; else use listings
    (progn
      (setq org-latex-listings t)
      (add-to-list 'org-latex-packages-alist '("" "listings"))
      (add-to-list 'org-latex-packages-alist '("" "color"))))

  ;; Add support for writing letters:
  (add-to-list 'org-latex-classes
               '("letter"
                 "\\documentclass[11pt]{letter}
\\signature{J. Alexander Branham}
\\address{}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(progn ; `paragraphs.el'
  ;; I end sentences with a single space.
  (setq sentence-end-double-space nil))

(use-package paren
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-delay 0)
  (show-paren-mode t))

(use-package password-store
  ;; I use pass to manage all my passwords and login info ---
  ;; https://www.passwordstore.org/
  :if (executable-find "pass")
  :mode ("\\.password-store/.*\\.gpg\\'" . text-mode)
  :custom
  (password-store-password-length 20 "Set longer default password length."))

(use-package pdf-tools
  ;; I like emacs, so why not view PDFs in it?  The built-in docview mode
  ;; can do so, but pdf-tools is better in all sorts of ways.

  ;; NOTE: ~pdf-tools~ only officially supports gnu/linux operating
  ;; systems. I think that it will work on macs as well, but you may have
  ;; to finagle it a bit. Regardless, I tell emacs to only use it if the OS
  ;; is linux based.
  :if (eq system-type 'gnu/linux)
  :magic ("%PDF" . pdf-view-mode)
  :defer 7
  :custom
  (pdf-sync-forward-display-pdf-key "<C-return>" "Use C-RET in latex mode to jump to location in pdf file")
  (pdf-view-display-size 'fit-page "Show full pages by default instead of fitting page width.")
  (TeX-view-program-selection '((output-pdf "pdf-tools")) "Use pdf-tools to display pdfs from latex runs.")
  (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  :config
  ;; The t says to install the server without asking me --- this may take a
  ;; second
  (pdf-tools-install t))

(use-package prog-mode
  ;; Prettify-symbols-mode will replace some symbols (like "lambda") with
  ;; their prettier cousins (like λ), but smartly as it's configured by
  ;; major modes themselves.
  :defer t
  :custom
  (prettify-symbols-unprettify-at-point 'right-edge)
  :config
  (global-prettify-symbols-mode))

(use-package projectile
  ;; Projectile makes using projects easier in emacs.  It also plays well
  ;; with helm, so let's set that up.
  :custom
  (projectile-completion-system 'helm)
  (projectile-require-project-root nil)
  :config
  (projectile-mode)
  (projectile-cleanup-known-projects))

(use-package python
  ;; The package is called python, the mode is python-mode. Confusingly, there's
  ;; also python-mode.el but I don't use that.
  :defer t
  :bind
  (:map python-mode-map
        ("C-<return>" . python-shell-send-region-or-statement-and-step))
  :custom
  ;; Use flake8 for flymake:
  (python-flymake-command '("flake8" "-"))
  (python-indent-guess-indent-offset-verbose nil)
  (python-indent-offset 4)
  :config
  (defun python-shell-send-region-or-statement ()
    "Send the current region to the inferior python process if there is an active one, otherwise the current line."
    (interactive)
    (if (use-region-p)
        (python-shell-send-region (region-beginning) (region-end))
      (python-shell-send-statement)))
  (defun python-shell-send-statement ()
    "Send the current line to the inferior python process for evaluation."
    (interactive)
    (save-excursion
      (let ((end (python-nav-end-of-statement))
            (beginning (python-nav-beginning-of-statement)))
        (python-shell-send-region beginning end))))
  (defun python-shell-send-region-or-statement-and-step ()
    "Call `python-shell-send-region-or-statement' and then `python-nav-forward-statement'."
    (interactive)
    (python-shell-send-region-or-statement)
    (python-nav-forward-statement))
  (define-minor-mode python-use-ipython-mode
    ;; I don't really get the allure of ipython, but here's something that
    ;; lets me switch back and forth:
    "Make python mode use the ipython interpreter."
    :lighter (" iPy")
    (unless (executable-find "ipython")
      (error "Could not find ipython executable"))
    (if python-use-ipython-mode
        ;; activate ipython stuff
        (setq python-shell-buffer-name "Ipython"
              python-shell-interpreter "ipython"
              ;; https://emacs.stackexchange.com/q/24453/115
              ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25306
              python-shell-interpreter-args "--simple-prompt -i")
      ;; else, deactivate everything
      (setq python-shell-buffer-name "Python"
            python-shell-interpreter "python"
            python-shell-interpreter-args "-i"))))

(use-package reftex
  ;; I use helm-bibtex to manage my references, but ReFTeX is still great
  ;; to have around for cross-references in latex files.
  :hook
  (LaTeX-mode . turn-on-reftex)
  :custom
  (reftex-cite-format
   '((?\C-m . "\\cite[]{%l}")
     (?t . "\\citet{%l}")
     (?p . "\\citep[]{%l}")
     (?a . "\\autocite{%l}")
     (?A . "\\textcite{%l}")
     (?P . "[@%l]")
     (?T . "@%l [p. ]")
     (?x . "[]{%l}")
     (?X . "{%l}")))
  (reftex-default-bibliography '("~/Sync/bibliography/references.bib"))
  (reftex-extra-bindings t))

(use-package saveplace
  ;; Yes, please save my place when opening/closing files:
  :config
  (save-place-mode))

(use-package sendmail
  :defer t
  :custom
  (send-mail-function #'smtpmail-send-it))

(use-package server
  :if window-system
  :config
  ;; Start the server if not already running:
  (unless (server-running-p)
    (add-hook 'after-init-hook #'server-start t)))

(use-package sh-script
  :defer t
  :mode
  ("PKGBUILD" . sh-mode))

(use-package shell
  :hook
  ;; Make urls clickable
  (shell-mode . goto-address-mode)
  :config
  (setenv "PAGER" "cat"))

(use-package simple
  :defer t
  :commands (my/toggle-window-split)
  :hook
  ;; Turn on visual line mode for nice line wrapping
  (after-init . global-visual-line-mode)
  :bind
  ("M-/" . cycle-spacing)
  (:prefix-map my/transpose-map
               :prefix "C-t"
               ("f" . my/toggle-window-split)
               ("c" . transpose-chars)
               ("w" . transpose-words)          ;also M-t by default
               ("l" . transpose-lines)
               ("p" . transpose-paragraphs)
               ("s" . transpose-sentences)
               ("x" . transpose-sexps))
  :custom
  (column-number-mode t "Turn on column numbers in mode-line.")
  (delete-active-region 'kill "Single char delete commands kill active regions.")
  (save-interprogram-paste-before-kill t "Save system clipboard before overwriting it.")
  (set-mark-command-repeat-pop t)
  (shell-command-dont-erase-buffer t "Don't erase output in shell buffers since it's so easy to navigate around.")
  (async-shell-command-display-buffer nil "Only show a shell buffer if there's something to show.")
  (kill-ring-max 500)
  :config
  (defun my/toggle-window-split ()
    "Switch between 2 windows split horizontally or vertically."
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))
  (defun my/extract-pdf-pages (infile frompg topg)
    "Extracts pages from a pdf file.

Extract pages from INFILE from FROMPG to TOPG using ghostscript.
Output file will be named by appending _pXX-pYY to INFILE."
    (interactive "ffile: \nnfrom: \nnto: ")
    (async-shell-command
     (concat "gs -sDEVICE=pdfwrite -dNOPAUSE -dBATCH -dSAFER"
             " -dFirstPage=" (number-to-string frompg)
             " -dLastPage=" (number-to-string topg)
             " -SOutputFile=" (concat
                               (file-name-sans-extension infile)
                               "_p" (number-to-string frompg)
                               "-p" (number-to-string topg)
                               ".pdf ")
             infile)))
  )                                     ; end use-package simple

(use-package smtpmail
  :hook
  (message-send . mbork/message-warn-if-no-attachments)
  :config
  ;; warn if no attachments
  (defun mbork/message-attachment-present-p ()
    "Return t if an attachment is found in the current message."
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (when (search-forward "<#part" nil t) t))))

  (defvar mbork/message-attachment-intent-re
    (regexp-opt '("I attach"
                  "I have attached"
                  "I've attached"
                  "I have included"
                  "I've included"
                  "see the attached"
                  "see the attachment"
                  "attached file"))
    "A regex which - if found in the message, and if there is no
attachment - should launch the no-attachment warning.")

  (defvar mbork/message-attachment-reminder
    "Are you sure you want to send this message without any attachment? "
    "The default question asked when trying to send a message
containing `mbork/message-attachment-intent-re' without an
actual attachment.")

  (defun mbork/message-warn-if-no-attachments ()
    "Ask the user if s?he wants to send the message even though
there are no attachments."
    (when (and (save-excursion
                 (save-restriction
                   (widen)
                   (goto-char (point-min))
                   (re-search-forward
                    mbork/message-attachment-intent-re nil t)))
               (not (mbork/message-attachment-present-p)))
      (unless (y-or-n-p mbork/message-attachment-reminder)
        (keyboard-quit)))))

(use-package spacemacs-dark-theme
  ;; By default, emacs starts with a blindingly white theme.  Let's get rid
  ;; of that pronto.
  :custom
  (spacemacs-theme-underline-parens nil)
  :init
  (defvar my/theme 'spacemacs-dark
    "The theme I'm using.")
  (defvar my/theme-window-loaded nil)
  (defvar my/theme-terminal-loaded nil)
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (if (window-system frame)
                      (unless my/theme-window-loaded
                        (if my/theme-terminal-loaded
                            (enable-theme my/theme)
                          (load-theme my/theme t))
                        (setq my/theme-window-loaded t))
                    (unless my/theme-terminal-loaded
                      (if my/theme-window-loaded
                          (enable-theme my/theme)
                        (load-theme my/theme t))
                      (setq my/theme-terminal-loaded t)))))
    (progn
      (load-theme my/theme t)
      (if (display-graphic-p)
          (setq my/theme-window-loaded t)
        (setq my/theme-terminal-loaded t)))))

(use-package stan-mode
  ;; stan is a language to write Bayesian models in
  :defer t
  :mode ("\\.stan\\'" . stan-mode))

(progn ; `subr'
  ;; Narrowing
  ;; Emacs has a great system to "narrow" a buffer to just a smaller bit. This is useful in a whole bunch of unexpected ways. For example, if a function will do something to a whole buffer but you only want to apply it to part, you can just narrow to that bit of the buffer. Or narrow just to one org subtree when you have a massive org document. The narrow commands are a bit confusing by default. This cleans them up a bit and makes it more intuitive to use. I got this from [[http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html][this post]] (modified a bit).

  (defun narrow-or-widen-dwim (p)
    "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
    (interactive "P")
    (declare (interactive-only))
    (cond ((and (buffer-narrowed-p) (not p)) (widen))
          ((region-active-p)
           (narrow-to-region (region-beginning)
                             (region-end)))
          ((derived-mode-p 'org-mode)
           (cond ((ignore-errors (org-narrow-to-block) t))
                 (t (org-narrow-to-subtree))))
          ((derived-mode-p 'latex-mode)
           (LaTeX-narrow-to-environment))
          (t (narrow-to-defun))))

  ;; This line actually replaces Emacs' entire narrowing
  ;; keymap, that's how much I like this command. Only
  ;; copy it if that's what you want.
  (bind-key* "C-x n" #'narrow-or-widen-dwim)

  )

(use-package sx
  ;; Stack Exchange is the place to go to get questions answered.  This
  ;; lets me search it from within Emacs.  It also sets it up to use sx.el
  ;; to open stack exchange links (instead of eww or firefox):
  :defer t
  :bind ("M-s x" . sx-search)
  :commands (sx-search sx-open-link)
  :init
  (push '(".*stackexchange.*" . sx-open-link) browse-url-browser-function)
  (push '(".*stackoverflow.*" . sx-open-link) browse-url-browser-function)
  )

(use-package system-packages
  ;; This is a collection of functions I wrote to help me manage installed
  ;; system packages with emacs. You can find the package on github:
  ;; https://github.com/jabranham/system-packages
  :bind
  (:prefix-map my/system-packages-map
               :prefix "<f12>"
               ("i" . system-packages-install)
               ("s" . system-packages-search)
               ("U" . system-packages-uninstall)
               ("D" . system-packages-list-dependencies-of)
               ("I" . system-packages-get-info)
               ("P" . system-packages-list-files-provided-by)
               ("u" . system-packages-update)
               ("O" . system-packages-remove-orphaned)
               ("l" . system-packages-list-installed-packages)
               ("C" . system-packages-clean-cache)
               ("L" . system-packages-log)
               ("v" . system-packages-verify-all-packages)
               ("V" . system-packages-verify-all-dependencies)))

(use-package tex-site
  ;; AuCTeX is better than the built in tex mode; let's use it.  This
  ;; demand adds almost nothing and ensures that auctex gets to set itself
  ;; up properly. That's necessary because of how weirdly it gets loaded.
  :demand t
  :custom
  (TeX-auto-save t)
  (TeX-electric-escape t)
  (TeX-electric-math t "Smart $ behavior")
  (TeX-electric-sub-and-superscript t)
  (TeX-parse-self t)
  (reftex-plug-into-AUCTeX t)
  (TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-mode t)
  (TeX-clean-confirm nil)
  ;; TeX-command-list by default contains a bunch of stuff I'll never
  ;; use. I use latexmk, xelatexmk, and View.  That's pretty much it.
  ;; Maybe one day I'll add "clean" back to the list.
  (TeX-command-list
   '(("latexmk" "latexmk -synctex=1 -pdf %s"
      TeX-run-compile nil t :help "Process file with latexmk")
     ("View" "%V" TeX-run-discard-or-function nil t :help "Run Viewer")
     ("xelatexmk" "latexmk -synctex=1 -xelatex %s"
      TeX-run-compile nil t :help "Process file with xelatexmk")))
  (TeX-command-default "latexmk" "Use latexmk by default.")
  (TeX-auto-local
   (expand-file-name "auctex/auto" no-littering-var-directory) "Stop littering everywhere with auto/ directories")
  :hook
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . reftex-mode)
  (LaTeX-mode . TeX-PDF-mode)
  :config
  ;; revert pdf from file after compilation finishes
  (use-package tex-buf
    :config
    (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))
  (use-package latex
    :bind
    (:map LaTeX-mode-map
          ("M-p" . outline-previous-visible-heading)
          ("M-n" . outline-next-visible-heading)
          ("<backtab>" . org-cycle))
    :config
    (push "\\.fdb_latexmk" LaTeX-clean-intermediate-suffixes)
    (push "\\.fls" LaTeX-clean-intermediate-suffixes)
    (push "\\.synctex.gz" LaTeX-clean-intermediate-suffixes)))

(use-package text-mode
  :hook
  (text-mode . my/dubcaps-mode)
  :commands (my/dubcaps-mode)
  :init
  (defun dcaps-to-scaps ()
    "Convert word in DOuble CApitals to Single Capitals."
    (interactive)
    (and (= ?w (char-syntax (char-before)))
         (save-excursion
           (and (if (called-interactively-p 'any)
                    (skip-syntax-backward "w")
                  (= -3 (skip-syntax-backward "w")))
                (let (case-fold-search)
                  (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
                (capitalize-word 1)))))
  (define-minor-mode my/dubcaps-mode
    "Toggle `my/dubcaps-mode'.

Converts words in DOuble CApitals to Single Capitals as you
type."
    :init-value nil
    :lighter (" DC")
    (if my/dubcaps-mode
        (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
      (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local))))

(use-package tooltip
  :defer t
  :config
  ;; If the mouse goes over a divider between windows, Emacs helpfully
  ;; tells you what pressing the mouse buttons will do.  This is a little
  ;; annoying, though, so let's disable it:
  (tooltip-mode -1))

(use-package tramp
  ;; TRAMP allows me to visit remote files in my local Emacs instance.  It's
  ;; pretty sweet.
  :defer t
  :custom
  (tramp-histfile-override t "Don't leave histfiles everywhere.")
  (tramp-default-method "ssh" "Use ssh by default."))

(use-package undo-tree
  ;; Emacs undo system is incredibly powerful but a bit confusing.  This
  ;; package has a great visualization system that helps out, bound to C-x
  ;; u by default.
  :demand t
  :bind
  ("C-z" . undo-tree-undo)
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  :config
  (global-undo-tree-mode))

(use-package unfill
  ;; fill-paragraph is nice, but emacs weirdly lacks a convenient way to
  ;; unfill paragraphs once they're filled.  This package adds that
  ;; functionality.
  :bind
  ([remap fill-paragraph] . unfill-toggle))

(use-package vc-git
  :defer t
  :config
  ;; Also, we can substitute the code fork icon from font awesome (which
  ;; you'll need to have installed) to have a pretty symbol instead of
  ;; "git:branch"
  (defun my-vc-git-mode-line-string (orig-fn &rest args)
    "Replace Git in modeline with font-awesome git icon via ORIG-FN and ARGS."
    (let ((str (apply orig-fn args)))
      (concat [#xF126] " " (substring-no-properties str 4))))

  (advice-add #'vc-git-mode-line-string :around #'my-vc-git-mode-line-string)
  )

(use-package vc-hooks
  :defer t
  :custom
  (vc-follow-symlinks t "Don't ask to follow symlinks.")
  (vc-make-backup-files t "Always make backup files.  Of everything.  Always."))

(use-package which-key
  ;; Which key shows key bindings for incomplete commands (prefixes) in a
  ;; neat popup.
  :defer 7
  :commands which-key-mode
  :config
  (which-key-mode))

(progn ; `window.el'
  ;; start maximized
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  ;; Don't split windows vertically
  (setq split-height-threshold nil)
  ;; resize windows:
  (bind-keys ("S-C-<left>" . shrink-window-horizontally)
             ("S-C-<right>" . enlarge-window-horizontally)
             ("S-C-<down>" . shrink-window)
             ("S-C-<up>" . enlarge-window))
  ;; These functions make splitting windows behave more like I want it to.
  ;; This way, calling C-x 2 or C-x 3 both splits the window *and* shows
  ;; the last buffer.
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
  (bind-keys ("C-x 2" . my/vsplit-last-buffer)
             ("C-x 3" . my/hsplit-last-buffer))
  ;; Rebind C-x 1 to be able to restore window layout

  ;; By default, C-x 1 deletes all windows except the currently focused
  ;; one.  I set it up here so that it stores the layout before deleting
  ;; the windows so that I can restore the layout by hitting C-x 1 again.
  ;; Stolen from
  ;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-windows-buffers.el
  (defvar my/toggle-one-window--buffer-name nil
    "Variable to store the name of the buffer for which the `my/toggle-one-window'
function is called.")
  (defvar my/toggle-one-window--window-configuration nil
    "Variable to store the window configuration before `my/toggle-one-window'
function was called.")
  (defun my/toggle-one-window (&optional force-one-window)
    "Toggles the frame state between deleting all windows other than
the current window and the windows state prior to that."
    (interactive "P")
    (if (or (not (one-window-p))
            force-one-window)
        (progn
          (setq my/toggle-one-window--buffer-name (buffer-name))
          (setq my/toggle-one-window--window-configuration (current-window-configuration))
          (delete-other-windows))
      (progn
        (when my/toggle-one-window--buffer-name
          (set-window-configuration my/toggle-one-window--window-configuration)
          (switch-to-buffer my/toggle-one-window--buffer-name)))))

  (bind-key "C-x 1" #'my/toggle-one-window)
  ;; To quickly access scratch press my/map f1:
  (defun my/get-scratch ()
    "Switch to scratch buffer."
    (interactive)
    (switch-to-buffer "*scratch*"))
  (bind-key "<f1>" #'my/get-scratch my/map))

(use-package winum
  :demand t
  ;; I can use winum to quickly jump from window to window.
  :bind*
  ("M-0" . winum-select-window-0-or-10)
  ("M-1" . winum-select-window-1)
  ("M-2" . winum-select-window-2)
  ("M-3" . winum-select-window-3)
  ("M-4" . winum-select-window-4)
  ("M-5" . winum-select-window-5)
  ("M-6" . winum-select-window-6)
  ("M-7" . winum-select-window-7)
  ("M-8" . winum-select-window-8)
  ("M-9" . winum-select-window-9)
  :custom
  (winum-scope 'frame-local)
  (winum-auto-setup-mode-line t)
  :custom-face
  (winum-face ((t (:foreground "yellow"))))
  :config
  (winum-mode))

(use-package with-editor
  ;; Use Emacs as the $EDITOR environmental variable:
  :hook
  ((shell-mode eshell-mode) . with-editor-export-editor)
  :config
  (shell-command-with-editor-mode))

(use-package ws-butler
  ;; Whitespace is evil.  Let's get rid of as much as possible.  But we
  ;; don't want to do this with files that already had whitespace (from
  ;; someone else's project, for example).  This mode will call
  ;; `whitespace-cleanup' before buffers are saved (but smartly)!
  :hook
  ((prog-mode ess-mode ledger-mode gitconfig-mode) . ws-butler-mode)
  :custom
  (ws-butler-keep-whitespace-before-point nil))

(use-package yasnippet
  ;; Yasnippet allows you to type an abbreviation and then expand it into a
  ;; template. We can look at yasnippet's documentation on github:
  ;; https://github.com/capitaomorte/yasnippet.

  ;; If I want to make my own, I can put them in `yas-snippet-dirs'

  ;; I integrate yasnippet with hippie-expand so using `hippie-expand'
  ;; expands a snippet if I have one, and then otherwise tries the
  ;; `hippie-expand' functions.

  :after hippie-exp
  :demand t
  :custom
  (yas-wrap-around-region t)
  (yas-prompt-functions '(yas-completing-prompt) "If competing snippets, use completing-read (helm) to select:")
  ;; (yas-alias-to-yas/prefix-p nil "Don't make old style yas/ symbols.")
  :init
  ;; disable yas minor mode map
  ;; use hippie-expand instead
  (setq yas-minor-mode-map (make-sparse-keymap))
  :config
  (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1)))
  (unbind-key "C-c &" yas-minor-mode-map)
  (yas-global-mode))

;; Local Variables:
;; indent-tabs-mode: nil
;; fill-column: 75
;; sentence-end-double-space: t
;; no-byte-compile: t
;; End:

;;; init.el ends here
