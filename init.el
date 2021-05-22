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
  (general-evil-setup t))

(use-package hydra)

;; Profile startup time
(use-package esup
  :init
  (setq esup-depth 0)
  :commands (esup))

;; UI
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-14" ))

(use-package nord-theme
  :config
  (load-theme 'nord t))

(use-package hl-line
  :config
  (global-hl-line-mode))

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

(use-package visual-regexp
  :config
  (general-define-key "s-r" 'vr/query-replace))

(general-define-key
 :states '(normal visual emacs)
 :prefix "["
 "SPC" 'ryche/insert-line-above)

(general-define-key
 :states '(normal visual emacs)
 :prefix "]"
 "SPC" 'ryche/insert-line-below)

(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (general-define-key :keymaps 'evil-normal-state-map "C-." nil)

  (with-eval-after-load 'evil-maps
    (define-key evil-normal-state-map (kbd "C-n") nil)
    (define-key evil-normal-state-map (kbd "C-p") nil)))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :after evil
  :config
  (general-define-key
   "M-;" 'evilnc-comment-or-uncomment-lines))

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

(use-package expand-region
  :config
  (general-define-key
   "C-=" 'er/expand-region
   "C--" 'er/contract-region))

(use-package avy
  :config
  (general-define-key "C-:" 'avy-goto-char)
  (general-define-key "C-;" 'avy-goto-char-2)
  (general-define-key "C-'" 'avy-goto-line))

(use-package undo-tree
  :init
  (global-undo-tree-mode 1))

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

(use-package format-all
  :commands format-all-buffer)

(general-define-key "C-f" 'format-all-buffer)

;; Search
(use-package rg
  :config
  (rg-enable-default-bindings)
  (general-define-key "s-F" nil)
  (general-define-key "s-F" 'rg-menu))

(use-package wgrep)

;; Windows and buffers
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode t)

(general-define-key
 "s-1" 'delete-other-windows
 "s-2" 'split-window-below
 "s-3" 'split-window-right
 "s-0" 'delete-window
 "s-w" 'delete-window)

(general-define-key
 "s->" 'next-buffer
 "s-<" 'previous-buffer)

(use-package ace-window
  :config
  (general-define-key "s-o" 'ace-window))

(use-package winner
  :straight (:type built-in)
  :after evil
  :config
  (winner-mode)
  (define-key evil-window-map "u" 'winner-undo)
  (define-key evil-window-map "U" 'winner-redo))

(use-package ibuffer
  :commands (ibuffer)
  :config
  (general-define-key
   "C-x C-b" 'ibuffer))

(use-package ibuffer-vc
  :after ibuffer
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
   "C-r" 'selectrum-select-from-history
   "C-j" 'selectrum-next-candidate
   "C-k" 'selectrum-previous-candidate
   "C-h" 'backward-kill-word))

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
  (general-define-key "s-F" nil)
  (general-define-key
   "s-F" 'consult-ripgrep
   "s-b" 'consult-buffer
   "s-B" 'consult-buffer-other-window))

(use-package marginalia
  :init
  (marginalia-mode))

;; File management
(recentf-mode 1)
(setq recentf-max-menu-items 25
      recentf-max-saved-items 25)

(setq vc-follow-symlinks t) ;; Follow symlinks without asking

(general-define-key
 "C-c ." 'ryche/edit-emacs-config
 "C-c >" 'ryche/reload-emacs-config)

(use-package dired
  :straight (:type built-in)
  :commands (dired dired-jump)
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired t
          insert-directory-program "/usr/local/bin/gls"))
  (setq-default dired-listing-switches "-lhvA --group-directories-first")
  (setq dired-clean-up-buffers-too t
        dired-recursive-copies 'always
        dired-recursive-deletes 'top)
  (put 'dired-find-alternate-file 'disabled nil))

(general-define-key "s-d" 'dired-jump)

(use-package dired-single
  :after dired
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-alternate-file))

(use-package dired-hide-dotfiles
  :after dired
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

(general-define-key "s-g" nil)
(general-define-key "s-g s" 'magit-status)

(use-package magit-todos
  :defer t)

(use-package git-link
  :commands git-link
  :config
  (setq git-link-open-in-browser t))

(general-define-key "s-g l" 'git-link)

(use-package git-timemachine
  :commands git-timemachine)

(general-define-key "s-g t" 'git-timemachine)

(use-package diff-hl
  :init
  (global-diff-hl-mode)
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))

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

  (flycheck-define-checker python-mypy
    "Mypy syntax and type checker."
    :command ("mypy"
              "--show-column-numbers"
              (config-file "--config-file" flycheck-python-mypy-config)
              (option "--cache-dir" flycheck-python-mypy-cache-dir)
              source-original)
    :working-directory flycheck-mypy--find-project-root
    :error-patterns
    ((error line-start (file-name) ":" line (optional ":" column)
            ": error:" (message) line-end)
     (warning line-start (file-name) ":" line (optional ":" column)
              ": warning:" (message) line-end)
     (info line-start (file-name) ":" line (optional ":" column)
           ": note:" (message) line-end))
    :modes python-mode
    ;; Ensure the file is saved, to work around
    ;; https://github.com/python/mypy/issues/4746.
    :predicate flycheck-buffer-saved-p)

  (add-hook 'lsp-after-initialize-hook
            (lambda ()
              (when (derived-mode-p 'python-mode)
                (flycheck-add-next-checker 'lsp 'python-mypy t)
                (message "Added flycheck checkers."))))

  (setq flycheck-highlighting-mode nil)

  (defhydra hydra-error (global-map "C-c e")
    "goto-error"
    ("n" flycheck-next-error "next")
    ("p" flycheck-previous-error "prev")
    ("h" flycheck-first-error "first")
    ("q" nil "quit"))
  (general-define-key
   "C-c e p" 'flycheck-previous-error
   "C-c e l" 'flycheck-list-errors))

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
  (general-define-key
   :keymaps 'python-mode-map
   "C-." 'lsp-find-definition
   "C-," 'lsp-find-references))

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
  :after python
  :config
  (setq py-isort-options '("-m=3")))

(use-package python-black
  :after python)

(use-package pyimport
  :after python)

(general-define-key :keymaps 'evil-motion-state-map "C-f" nil)
(general-define-key
 :keymaps 'python-mode-map
 "C-f" 'ryche/format-python)

(use-package web-mode
  :mode "\\.html?\\'"
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-expanding t))

(use-package lispy
  :defer t)

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package haml-mode
  :mode "\\.haml\\'")

(use-package racket-mode
  :defer t)

;; Jump to definition everywhere
(use-package dumb-jump
  :commands (xref-find-definitions xref-find-references)
  :config
  (setq dumb-jump-selector 'selectrum)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(general-define-key
 "C-." 'xref-find-definitions
 "C-," 'xref-find-references)

;; Custom iterm package
(use-package iterm
  :straight (:type built-in)
  :load-path "lisp/iterm")

;; Custom functions
(defun ryche/edit-emacs-config ()
  "Open Emacs configuration file."
  (interactive)
  (find-file user-init-file))

(defun ryche/reload-emacs-config ()
  "Reload Emacs configuration file."
  (interactive)
  (find-file user-init-file)
  (save-buffer)
  (load-file user-init-file))

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

(defun ryche/format-python ()
  "Make python buffer pretty."
  (interactive)
  (save-buffer)
  (pyimport-remove-unused)
  (py-isort-buffer)
  (python-black-buffer))


(provide 'init)

;;; init.el ends here
