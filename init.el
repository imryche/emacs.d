(setq delete-old-versions -1
      inhibit-startup-screen t
      ring-bell-function 'ignore
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8
      sentence-end-double-space nil
      fill-column 80
      initial-scratch-message ""
      frame-resize-pixelwise t
      make-backup-files nil
      create-lockfiles nil
      auto-save-default nil
      ediff-window-setup-function 'ediff-setup-windows-plain)

(setq default-directory (concat (getenv "HOME") "/"))

;; Garbage collection
(setq gc-cons-threshold 20000000)
(add-function :after after-focus-change-function #'garbage-collect)

(global-hl-line-mode)

(setq byte-compile-warnings '(cl-functions))
(setq-default indent-tabs-mode nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(electric-pair-mode)

;; Highlight current line in prog mode
(add-hook 'prog-mode-hook 'hl-line-mode)

;; Text mode customizations
(add-hook 'text-mode-hook #'auto-fill-mode)

;; Store customizations in the separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; yes no -> y n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Font
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-14" ))

;; Package Management
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Always compile packages and use the newest version available
(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

(use-package general
  :config
  (general-create-definer set-leader-keys :prefix "SPC")
  (general-create-definer set-local-leader-keys :prefix ","))

(use-package esup
  :init
  (setq esup-depth 0)
  :ensure t
  :pin melpa
  :commands (esup))

(use-package restart-emacs
  :init
  (set-leader-keys
    :states '(normal visual emacs)
    "qr" 'restart-emacs))

;; (use-package color-theme-sanityinc-tomorrow
;;   :config
;;   (load-theme 'sanityinc-tomorrow-night t))

(use-package nord-theme
  :config
  (load-theme 'nord t))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-major-mode-icon nil
        doom-modeline-buffer-modification-icon nil))

(use-package minions
  :config
  (setq minions-mode-line-lighter ""
        minions-mode-line-delimiters '("" . ""))
  (minions-mode 1))

(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-undo-system 'undo-tree)
  (global-undo-tree-mode)
  :config
  (evil-mode 1))

(use-package undo-tree)

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :init
  (set-leader-keys
    :states '(normal visual emacs)
    ";" 'evilnc-comment-or-uncomment-lines))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

;; Switch/Move windows
(use-package ace-window
  :init
  (set-leader-keys
    :states '(normal visual emacs)
    "o" 'ace-window))

(use-package buffer-move
  :init
  (set-leader-keys
    :states '(normal visual emacs)
    "wmh" 'buf-move-left
    "wmj" 'buf-move-down
    "wmk" 'buf-move-up
    "wml" 'buf-move-right))

;; Ivy
(use-package counsel
  :diminish (ivy-mode . "") ; does not display ivy in the modeline
  :init
  (ivy-mode 1)        ; enable ivy globally at startup
  (general-define-key
   :keymaps 'ivy-mode-map
   "C-h" 'delete-backward-char
   "C-j" 'ivy-next-line
   "C-k" 'ivy-previous-line
   "C-l" 'ivy-alt-done)
  (set-leader-keys
    :states '(normal visual emacs)
    "/" 'counsel-rg
    "?" 'oneor0/counsel-rg-thing-at-point
    "x" 'counsel-M-x
    "SPC" 'counsel-ibuffer
    "ff" 'counsel-find-file
    "hv" 'counsel-describe-variable
    "hf" 'counsel-describe-function
    "sr" 'ivy-resume)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 20)
  (setq ivy-count-format "(%d/%d) "))

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1))

(use-package ivy-xref
  :ensure t
  :init
  (setq xref-show-definitions-function #'ivy-xref-show-defs)
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

(use-package avy
  :commands
  (avy-goto-word-1)
  :init
  (set-leader-keys
    :states '(normal visual emacs)
    "jl" 'avy-goto-line
    "jf" 'avy-goto-char-timer
    "jw" 'avy-goto-word-1
    "jr" 'avy-resume))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :init
  (set-leader-keys
    :states '(normal visual emacs)
    "fd" 'dired-jump
    "fD" 'dired-jump-other-window)
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired t
          insert-directory-program "/usr/local/bin/gls"))
  (setq-default dired-listing-switches "-lhvA --group-directories-first")
  (setq dired-clean-up-buffers-too t
        dired-recursive-copies 'always
        dired-recursive-deletes 'top))

(use-package dired-single
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-alternate-file))

(use-package dired-hide-dotfiles
  :config
  (dired-hide-dotfiles-mode)
  (evil-collection-define-key 'normal 'dired-mode-map
    "." 'dired-hide-dotfiles-mode))

(use-package ibuffer
  :init
  (set-leader-keys
    :states '(normal visual emacs)
    "bb" 'ibuffer))

(use-package ibuffer-vc
  :config
  (add-hook 'ibuffer-hook 'ibuffer-vc-set-filter-groups-by-vc-root))

(use-package magit
  :defer t
  :init
  (set-leader-keys
    :states '(normal visual emacs)
    "gs" 'magit-status
    "gb" 'magit-blame)
  :config
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

(use-package git-timemachine
  :init
  (set-leader-keys
    :states '(normal visual emacs)
    "gt" 'git-timemachine))

(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package projectile
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode))

(use-package counsel-projectile
  :init
  (set-leader-keys
    :states '(normal visual emacs)
    "pp" 'counsel-projectile-switch-project
    "pf" 'counsel-projectile-find-file
    "pd" 'counsel-projectile-find-dir
    "pb" 'counsel-projectile-switch-to-buffer)
  :config
  (counsel-projectile-mode))

(use-package format-all
  :init
  (set-leader-keys
    :states '(normal visual emacs)
    "=" 'format-all-buffer))

(use-package whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode t))

(use-package org
  :defer t
  :ensure org-plus-contrib
  :init
  (setq org-log-done 'time)
  (setq org-directory "~/Dropbox/org")
  (setq org-agenda-files (list "~/Dropbox/org/projects/"))
  (setq org-link-frame-setup '((file . find-file)))
  :config
  (add-hook 'org-mode-hook 'visual-line-mode)
  (defun org-file-path (filename)
    "Return the absolute address of an org file, given its relative name."
    (concat (file-name-as-directory org-directory) filename)))

(use-package org-bullets
  :defer t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-roam
  :init
  (set-leader-keys
    :states '(normal visual emacs)
    "rr" 'org-roam
    "rf" 'org-roam-find-file
    "rg" 'org-roam-graph)
  :bind (:map org-mode-map
              (("C-c r i" . org-roam-insert))
              (("C-c r I" . org-roam-insert-immediate)))
  :config
  (setq org-roam-directory "~/Dropbox/org-roam")
  (setq org-roam-graph-viewer "/usr/bin/open")
  (require 'org-roam-protocol))

(use-package markdown-mode
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package company
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0) ;; default is 0.2
  (global-company-mode t))

(use-package dumb-jump
  :init
  (set-leader-keys
    :states '(normal visual emacs)
    "." 'xref-find-definitions
    "," 'xref-pop-marker-stack)
  :config
  (setq dumb-jump-selector 'ivy)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package super-save
  :config
  (super-save-mode +1))

(use-package lispy :defer t)

(use-package flymake
  :ensure nil
  :init
  (set-leader-keys
    :states '(normal visual emacs)
    "en" 'flymake-goto-next-error
    "ep" 'flymake-goto-prev-error)
  :config
  (custom-set-faces
   '(flymake-warning ((t (:underline nil))))
   '(flymake-error ((t (:underline nil))))))

(use-package eglot
  :init
  (setq eldoc-documentation-functions '(eglot-signature-eldoc-function))
  (setq eglot-workspace-configuration
        '((:pyls . ((configurationSources . ["flake8"])
                    (:plugins (:flake8 (:enabled . t)))))))
  (setq eglot-ignored-server-capabilites '(list :documentHighlightProvider :hoverProvider))
  :hook
  (python-mode . eglot-ensure)
  :config
  (custom-set-faces
   '(eglot-highlight-symbol-face ((t (:inherit nil)))))
  (set-leader-keys
    :states '(normal visual emacs)
    :keymaps 'python-mode-map
    "." 'xref-find-definitions
    "," 'xref-pop-marker-stack))

(use-package py-isort
  :init
  (setq py-isort-options '("-m=3"))
  :config
  (set-local-leader-keys
    :keymaps 'python-mode-map
    :states '(normal visual emacs)
    "ss" 'py-isort-buffer))

(use-package blacken
  :config
  (set-local-leader-keys
    :keymaps 'python-mode-map
    :states '(normal visual emacs)
    "b" 'blacken-buffer))

(use-package web-mode)

(use-package yaml-mode)

(use-package dockerfile-mode)

(use-package racket-mode
  :defer t
  :config
  (set-local-leader-keys
    :keymaps 'racket-mode-map
    :states '(normal visual emacs)
    "'" 'racket-repl
    "sb" 'racket-run
    "sr" 'racket-send-region))

(require 'iterm)
(set-local-leader-keys
  :keymaps 'python-mode-map
  :states '(normal visual emacs)
  "t" 'iterm-pytest
  "T" 'iterm-pytest-file)

;; Custom functions
(defun oneor0/edit-emacs-config ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun ivy-with-thing-at-point (cmd)
  (let ((ivy-initial-inputs-alist
         (list
          (cons cmd (thing-at-point 'symbol)))))
    (funcall cmd)))

(defun oneor0/counsel-rg-thing-at-point ()
  (interactive)
  (ivy-with-thing-at-point 'counsel-rg))

(defun swiper-thing-at-point ()
  (interactive)
  (ivy-with-thing-at-point 'swiper))

(defun oneor0/project-tasks ()
  (interactive)
  (find-file (concat (projectile-project-root) "tasks.org")))

(defun oneor0/split-right-switch ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun oneor0/split-below-switch ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun oneor0/autoflake-buffer()
  "autoflake --remove-all-unused-imports -i unused_imports.py"
  (interactive)
  (if (executable-find "autoflake")
      (progn
        (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                               (shell-quote-argument (buffer-file-name))))
        (revert-buffer t t t))
    (message "Error: Cannot find autoflake executable.")))

(defun insert-line-above ()
  "Insert an empty line above the current line."
  (interactive)
  (save-excursion
    (end-of-line 0)
    (open-line 1)))

(defun insert-line-below ()
  "Insert an empty line below the current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (open-line 1)))

;; Keybindings
(general-define-key
 :states '(normal visual emacs)
 "/" 'swiper
 "?" 'swiper-thing-at-point)

(general-define-key
 :states '(normal visual emacs)
 :prefix "["
 "SPC" 'insert-line-above)

(general-define-key
 :states '(normal visual emacs)
 :prefix "]"
 "SPC" 'insert-line-below)

(set-leader-keys
  :states '(normal visual emacs)
  "TAB" 'mode-line-other-buffer
  "qq" 'save-buffers-kill-emacs
  "fs" 'save-buffer
  "fS" (lambda () (interactive)(save-some-buffers t))
  "f." 'oneor0/edit-emacs-config
  "bd" 'kill-current-buffer
  "wl" 'windmove-right
  "wh" 'windmove-left
  "wk" 'windmove-up
  "wj" 'windmove-down
  "w/" 'oneor0/split-right-switch
  "w-" 'oneor0/split-below-switch
  "wd" 'delete-window
  "ww" 'ace-delete-other-windows)
