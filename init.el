(setq delete-old-versions -1
      inhibit-startup-screen t
      ring-bell-function 'ignore
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8
      sentence-end-double-space nil
      default-fill-column 80
      initial-scratch-message ""
      frame-resize-pixelwise t
      make-backup-files nil
      create-lockfiles nil
      auto-save-default nil
      ediff-window-setup-function 'ediff-setup-windows-plain)

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

;; Store customizations in the separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Start server
(require 'server)
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

;; yes no -> y n
(defalias 'yes-or-no-p 'y-or-n-p)

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

;; General
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

;; Font
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-14" ))

;; Theme
(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-night t))

;; Hide all minor modes
(use-package minions
  :config
  (setq minions-mode-line-lighter ""
	minions-mode-line-delimiters '("" . ""))
  (minions-mode 1))

;; Evil
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
  :config
  (evil-collection-init))

(use-package evil-escape
  :config
  (evil-escape-mode))

(use-package evil-nerd-commenter)

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-org
  :ensure t
  :after org
  :init
  (defun oneor0/org-mode-hook ()
    (set-local-leader-keys
      :keymaps 'org-mode-map
      :states '(normal visual emacs)
      "t" 'counsel-org-tag))
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme)))
  (add-hook 'evil-org-mode-hook 'oneor0/org-mode-hook)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Path management
(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

;; Switch/Move windows
(use-package ace-window
  :config
  (set-leader-keys
    :states '(normal visual emacs)
    "o" 'ace-window))

(use-package beacon
  :config
  (beacon-mode 1))

(use-package buffer-move
  :config
  (set-leader-keys
    :states '(normal visual emacs)
    "wmh" 'buf-move-left
    "wmj" 'buf-move-down
    "wmk" 'buf-move-up
    "wml" 'buf-move-right))

;; Ivy
(use-package ivy
  :diminish (ivy-mode . "") ; does not display ivy in the modeline
  :init (ivy-mode 1)        ; enable ivy globally at startup
  :bind (:map ivy-mode-map
	      ("C-h" . 'delete-backward-char)
	      ("C-j" . 'ivy-next-line)
	      ("C-k" . 'ivy-previous-line)
	      ("C-l" . 'ivy-alt-done))
  :config
  (setq ivy-use-virtual-buffers t)   ; extend searching to bookmarks and …
  (setq ivy-height 20)               ; set height of the ivy window
  (setq ivy-count-format "(%d/%d) ") ; count format, from the ivy help page
  )

(use-package counsel
  :config
  (use-package flx)
  (use-package smex))

(use-package which-key
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

(use-package avy
  :commands (avy-goto-word-1))

;; Ranger
(use-package ranger
  :init
  (setq ranger-show-hidden t))

;; iBuffer
(use-package ibuffer)

(use-package ibuffer-vc
  :config
  (add-hook 'ibuffer-hook 'ibuffer-vc-set-filter-groups-by-vc-root))

;; Git
(use-package magit
  :defer t
  :config
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

(use-package git-timemachine)

(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

;; Project management
(use-package projectile
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

;; Org
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
    (concat (file-name-as-directory org-directory) filename))
  (defun edit-work-tasks ()
    (interactive)
    (find-file (org-file-path "work.org"))))

(use-package org-bullets
  :defer t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-roam
  :init
  (setq org-roam-directory "~/Dropbox/org-roam")
  (setq org-roam-graph-viewer "/usr/bin/open")
  :bind (:map org-mode-map
              (("C-c r i" . org-roam-insert))
              (("C-c r I" . org-roam-insert-immediate)))
  :config
  (require 'org-roam-protocol))

;; Markdown
(use-package markdown-mode
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Autocompletion
(use-package company
  :config
  (global-company-mode t))

(use-package flycheck
  :init
  (global-flycheck-mode))

(use-package dumb-jump
  :config
  (set-leader-keys
    :states '(normal visual emacs)
    "." 'dumb-jump-go
    ">" 'dumb-jump-go-other-window)
  (setq dumb-jump-selector 'ivy))

;; Autosave
(use-package super-save
  :config
  (super-save-mode +1))

;; Lisp
(use-package lispy
  :defer t)

;; Python
(use-package anaconda-mode
  :defer t
  :config
  (defun oneor0/python-mode-hook ()
    (set-local-leader-keys
      :keymaps 'python-mode-map
      :states '(normal visual emacs)
      "g" 'anaconda-mode-find-definitions
      "G" 'anaconda-mode-find-definitions-other-window
      "a" 'anaconda-mode-find-assignments
      "A" 'anaconda-mode-find-assignments-other-window
      "r" 'anaconda-mode-find-references
      "R" 'anaconda-mode-find-references-other-window
      "?" 'anaconda-mode-show-doc
      "ss" 'py-isort-buffer
      "sr" 'oneor0/autoflake-buffer
      "tt" 'iterm-pytest
      "tf" 'iterm-pytest-file))
  (add-hook 'python-mode-hook 'oneor0/python-mode-hook)
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :config
  (eval-after-load "company" '(add-to-list 'company-backends 'company-anaconda)))

(use-package auto-virtualenvwrapper
  :init
  (require 'auto-virtualenvwrapper)
  :config
  (add-hook 'python-mode-hook #'auto-virtualenvwrapper-activate))

(use-package py-isort
  :init
  (setq py-isort-options '("-m=3")))

;; Web
(use-package web-mode)
(use-package emmet-mode)

;; Yaml
(use-package yaml-mode)

;; Docker
(use-package dockerfile-mode)

;; Racket
(use-package racket-mode
  :defer t
  :init
  (defun oneor0/racket-mode-hook ()
    (set-local-leader-keys
      :keymaps 'racket-mode-map
      :states '(normal visual emacs)
      "'" 'racket-repl
      "sb" 'racket-run
      "sr" 'racket-send-region))
  :config
  (add-hook 'racket-mode-hook 'oneor0/racket-mode-hook))

;; iTerm
(require 'iterm)

;; Custom functions
(defun edit-emacs-config ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun ivy-with-thing-at-point (cmd)
  (let ((ivy-initial-inputs-alist
	 (list
	  (cons cmd (thing-at-point 'symbol)))))
    (funcall cmd)))

(defun counsel-ag-thing-at-point ()
  (interactive)
  (ivy-with-thing-at-point 'counsel-ag))

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
  "/" 'counsel-ag
  "?" 'counsel-ag-thing-at-point
  "x" 'counsel-M-x
  "TAB" 'mode-line-other-buffer
  "SPC" 'counsel-ibuffer
  ";" 'evilnc-comment-or-uncomment-lines
  ;; File manager
  "dr" 'ranger
  "dd" 'deer
  ;; Quit
  "qq" 'save-buffers-kill-emacs
  ;; Files
  "fs" 'save-buffer
  "fd" 'deft
  "fS" (lambda () (interactive)(save-some-buffers t))
  "ff" 'counsel-find-file
  "f." 'edit-emacs-config
  "fow" 'edit-work-tasks
  ;; Buffers
  "bd" 'kill-current-buffer
  "bb" 'ibuffer
  ;; Search
  "sr" 'ivy-resume
  ;; Jump
  "jl" 'avy-goto-line
  "jf" 'avy-goto-char-timer
  "jw" 'avy-goto-word-1
  "jr" 'avy-resume
  ;; Git
  "gs" 'magit-status
  "gb" 'magit-blame
  ;; Window
  "wl" 'windmove-right
  "wh" 'windmove-left
  "wk" 'windmove-up
  "wj" 'windmove-down
  "w/" 'oneor0/split-right-switch
  "w-" 'oneor0/split-below-switch
  "wd" 'delete-window
  "ww" 'ace-delete-other-windows
  ;; Projects
  "pp" 'counsel-projectile-switch-project
  "pf" 'counsel-projectile
  "pb" 'counsel-projectile-switch-to-buffer
  "pt" 'oneor0/project-tasks
  ;; Help
  "hv" 'counsel-describe-variable
  "hf" 'counsel-describe-function
  ;; Org
  "at" 'org-todo-list
  "aa" 'org-agenda
  "al" 'org-store-link
  ;; Roam
  "rr" 'org-roam
  "rf" 'org-roam-find-file
  "rg" 'org-roam-graph)
