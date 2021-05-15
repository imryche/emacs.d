;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration

;;; Code:

(setq byte-compile-warnings '(cl-functions))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Store customizations in the separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Sensible defaults
(setq inhibit-startup-message t)

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

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(show-paren-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p) ;; yes no -> y n

;; Set default directory
(setq default-directory (expand-file-name "~/"))

;; Garbage collection
(setq gc-cons-threshold 20000000)
(add-function :after after-focus-change-function #'garbage-collect)

;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Maximize window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

;; Always compile packages and use the newest version available
(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

;; Simplify key bindings
(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer ryche/define-leader-keys :prefix ",")
  (general-create-definer ryche/define-super-keys :prefix "SPC"))

(use-package hydra
  :defer 1)

;; Profile startup time
(use-package esup
  :init
  (setq esup-depth 0)
  :commands (esup))

;; Restart
(use-package restart-emacs
  :init
  (ryche/define-super-keys
    :states '(normal visual emacs)
    "qr" 'restart-emacs))

;; UI
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-14" ))

(use-package nord-theme
  :config
  (load-theme 'nord t))

(use-package hl-line
  :config
  (add-hook 'prog-mode-hook 'hl-line-mode))

(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-major-mode-icon nil
        doom-modeline-buffer-modification-icon nil))

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-separator " "
        which-key-prefix-prefix "+"
        which-key-idle-delay 0.3))

(use-package minions
  :config
  (setq minions-mode-line-lighter ""
        minions-mode-line-delimiters '("" . ""))
  (minions-mode 1))

;; Editing
(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)

(use-package evil
  :defer .1
  :init
  (setq evil-want-keybinding nil
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (with-eval-after-load 'evil-maps
    (define-key evil-normal-state-map (kbd "C-n") nil)
    (define-key evil-normal-state-map (kbd "C-p") nil)))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :init
  (ryche/define-super-keys
    :states '(normal visual emacs)
    ";" 'evilnc-comment-or-uncomment-lines))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package avy
  :commands
  (avy-goto-word-1)
  :init
  (ryche/define-super-keys
    :states '(normal visual emacs)
    "jj" 'avy-goto-char
    "jl" 'avy-goto-line
    "jf" 'avy-goto-char-timer
    "jw" 'avy-goto-word-0
    "jr" 'avy-resume))

(use-package undo-tree
  :init
  (global-undo-tree-mode 1))

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(use-package format-all
  :init
  (ryche/define-super-keys
    :states '(normal visual emacs)
    "=" 'format-all-buffer))

(use-package wgrep)

;; Windows and buffers
(use-package ace-window
  :init
  (ryche/define-super-keys
    :states '(normal visual emacs)
    "o" 'ace-window))

(use-package buffer-move
  :init
  (ryche/define-super-keys
    :states '(normal visual emacs)
    "wmh" 'buf-move-left
    "wmj" 'buf-move-down
    "wmk" 'buf-move-up
    "wml" 'buf-move-right))

(use-package ibuffer
  :init
  (ryche/define-super-keys
    :states '(normal visual emacs)
    "bb" 'ibuffer))

(use-package ibuffer-vc
  :config
  (add-hook 'ibuffer-hook 'ibuffer-vc-set-filter-groups-by-vc-root))

;; Completion system
(use-package selectrum
  :custom
  (selectrum-fix-minibuffer-height t)
  :config
  (selectrum-mode +1)
  (general-define-key
   :keymaps 'selectrum-minibuffer-map
   "C-j" 'selectrum-next-candidate
   "C-k" 'selectrum-previous-candidate))

(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package consult
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-project-root-function #'projectile-project-root)
  (general-define-key
   :states '(normal visual emacs)
   "/" 'consult-line
   "?" 'consult-imenu)
  (ryche/define-super-keys
    :states '(normal visual emacs)
    "/" 'consult-ripgrep
    "SPC" 'consult-buffer))

(use-package marginalia
  :init
  (marginalia-mode))

;; File management
(setq vc-follow-symlinks t) ;; Follow symlinks without asking

(use-package dired
  :straight (:type built-in)
  :commands (dired dired-jump)
  :init
  (ryche/define-super-keys
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
        dired-recursive-deletes 'top)
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-single
  :after evil
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-alternate-file))

(use-package dired-hide-dotfiles
  :after evil
  :config
  (dired-hide-dotfiles-mode)
  (evil-collection-define-key 'normal 'dired-mode-map
    "." 'dired-hide-dotfiles-mode))

(use-package super-save
  :defer 1
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(use-package saveplace
  :hook ((after-init . save-place-mode)))

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

;; Git
(use-package magit
  :commands (magit-status)
  :config
  (setq magit-completing-read-function #'selectrum-completing-read)
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

(ryche/define-super-keys
  :states '(normal visual emacs)
  "g" '(:ignore t :which-key "git")
  "gs" 'magit-status
  "gd" 'magit-diff-unstaged
  "gc" 'magit-branch-or-checkout
  "gl" '(:ignore t :which-key "log")
  "glc" 'magit-log-current
  "glf" 'magit-log-buffer-file
  "gb" 'magit-branch
  "gP" 'magit-push-current
  "gp" 'magit-pull-branch
  "gf" 'magit-fetch
  "gF" 'magit-fetch-all
  "gr" 'magit-rebase)

(use-package magit-todos
  :defer t)

(use-package git-link
  :commands git-link
  :config
  (setq git-link-open-in-browser t))

(ryche/define-super-keys
  :states '(normal visual emacs)
  "gL" 'git-link)

(use-package git-timemachine
  :defer t
  :init
  (ryche/define-super-keys
    :states '(normal visual emacs)
    "gt" 'git-timemachine))

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package projectile
  :config
  (projectile-mode)
  (ryche/define-super-keys
    :states '(normal visual emacs)
    "pp" 'projectile-switch-project
    "pf" 'projectile-find-file
    "pr" 'projectile-replace
    "pR" 'projectile-replace-regexp))

;; Writing
(use-package darkroom
  :commands darkroom-mode)

(use-package org
  :defer t
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

(use-package markdown-mode
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

;; Syntax checking
(use-package flycheck
  :init
  (global-flycheck-mode)
  :config
  (defun flycheck-mypy--find-project-root (_checker)
    (and buffer-file-name
         (flycheck--locate-dominating-file-matching
          (file-name-directory buffer-file-name)
          (rx-to-string
           `(: bos (or ,@flycheck-python-mypy-config) eos)
           t))))

  (flycheck-define-checker
      python-mypy ""
      :command ("mypy"
                (config-file "--config-file" flycheck-python-mypy-config)
                (option "--cache-dir" flycheck-python-mypy-cache-dir)
                source-original)
      :working-directory flycheck-mypy--find-project-root
      :error-patterns
      ((error line-start (file-name) ":" line ": error:" (message) line-end))
      :modes python-mode)

  (add-to-list 'flycheck-checkers 'python-mypy t)

  (add-hook 'lsp-after-initialize-hook
            (lambda ()
              (when (derived-mode-p 'python-mode)
                (flycheck-add-next-checker 'lsp 'python-mypy t)
                (message "Added flycheck checkers."))))

  (setq flycheck-highlighting-mode nil)

  (ryche/define-super-keys
    :states '(normal visual emacs)
    "en" 'flycheck-next-error
    "ep" 'flycheck-previous-error
    "el" 'flycheck-list-errors))

;; Autocompletion
(use-package company
  :hook ((prog-mode . company-mode))
  :config
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 1))

;; Languages
(use-package lsp-mode
  :hook ((python-mode . lsp)
         (ruby-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-enable-symbol-highlighting nil
        lsp-lens-enable nil
        lsp-semantic-tokens-enable nil
        lsp-eldoc-enable-hover nil
        lsp-modeline-diagnostics-enable nil
        lsp-signature-render-documentation nil)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.mypy_cache\\'")
  (ryche/define-super-keys
    :states '(normal visual emacs)
    :keymaps 'python-mode-map
    "." 'lsp-find-definition
    "," 'lsp-find-references))

(use-package lsp-pyright
  :defer t
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (lsp-deferred)))
  :config
  (setq lsp-pyright-typechecking-mode "off"))

(use-package auto-virtualenv
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
  (add-hook 'window-configuration-change-hook 'auto-virtualenv-set-virtualenv))

(use-package py-isort
  :init
  (setq py-isort-options '("-m=3"))
  :config
  (add-hook 'before-save-hook 'py-isort-before-save))

(use-package blacken
  :config
  (add-hook 'python-mode-hook 'blacken-mode))

(use-package pyimport)

(use-package web-mode
  :mode "\\.html?\\'"
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-expanding t))

(use-package lispy
  :defer t)

(use-package yaml-mode)

(use-package dockerfile-mode)

(use-package racket-mode
  :defer t
  :config
  (ryche/define-leader-keys
    :keymaps 'racket-mode-map
    :states '(normal visual emacs)
    "'" 'racket-repl
    "sb" 'racket-run
    "sr" 'racket-send-region))

;; Jump to definition everywhere
(use-package dumb-jump
  :init
  (ryche/define-super-keys
    :states '(normal visual emacs)
    "." 'xref-find-definitions
    "," 'xref-pop-marker-stack)
  :config
  (setq dumb-jump-selector 'selectrum)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Custom iterm package
(use-package iterm
  :straight (:type built-in)
  :load-path "lisp/iterm"
  :config
  (ryche/define-leader-keys
    :keymaps 'python-mode-map
    :states '(normal visual emacs)
    "t" 'iterm-pytest
    "T" 'iterm-pytest-file))

;; Custom functions
(defun ryche/edit-emacs-config ()
  "Open Emacs configuration file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun ryche/thing-at-point (cmd)
  "Call a CMD with argument at point."
  (let ((ivy-initial-inputs-alist
         (list
          (cons cmd (thing-at-point 'symbol)))))
    (funcall cmd)))

(defun ryche/consult-ripgrep-thing-at-point ()
  "Ripgrep with thing at point."
  (interactive)
  (ryche/thing-at-point 'consult-ripgrep))

(defun ryche/consult-thing-at-point ()
  "Consult-line with thing at point."
  (interactive)
  (ryche/thing-at-point 'consult-line))

(defun ryche/split-right-switch ()
  "Split vertically and switch window immediately."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun ryche/split-below-switch ()
  "Split horizontally and switch window immediately."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun ryche/insert-line-above ()
  "Insert an empty line above the current line."
  (interactive)
  (save-excursion
    (end-of-line 0)
    (open-line 1)))

(defun ryche/insert-line-below ()
  "Insert an empty line below the current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (open-line 1)))

;; Keybindings
(general-define-key
 :states '(normal visual emacs)
 :prefix "["
 "SPC" 'ryche/insert-line-above)

(general-define-key
 :states '(normal visual emacs)
 :prefix "]"
 "SPC" 'ryche/insert-line-below)

(ryche/define-super-keys
  :states '(normal visual emacs)
  "TAB" 'mode-line-other-buffer
  "x" 'execute-extended-command
  "qq" 'save-buffers-kill-emacs
  "r" 'query-replace
  "R" 'query-replace-regexp
  "ff" 'find-file
  "fs" 'save-buffer
  "fS" (lambda () (interactive)(save-some-buffers t))
  "f." 'ryche/edit-emacs-config
  "bd" 'kill-current-buffer
  "wl" 'windmove-right
  "wh" 'windmove-left
  "wk" 'windmove-up
  "wj" 'windmove-down
  "w/" 'ryche/split-right-switch
  "w-" 'ryche/split-below-switch
  "wd" 'delete-window
  "ww" 'ace-delete-other-windows)

(provide 'init)

;;; init.el ends here
