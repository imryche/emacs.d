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

;; Garbage collection
(setq gc-cons-threshold (* 512 1024 1024)
      gc-cons-percentage 0.7
      garbage-collection-messages nil)

;; Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

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

;; To use Ctrl+hjkl as arrow keys system-wide, I need to remap help keybindings
(general-define-key "C-?" help-map)

;; More convenient M-x
(general-define-key "s-x" 'execute-extended-command)

;; Profile startup time
(use-package esup
  :init
  (setq esup-depth 0)
  :commands (esup))

;; UI
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-14" ))

;; (use-package nord-theme
;;   :config
;;   (load-theme 'nord t))

(use-package twilight-bright-theme
  :config
  (load-theme 'twilight-bright t))

(use-package hl-line
  :config
  (global-hl-line-mode))

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

(use-package visual-regexp
  :config
  (general-define-key "s-r" 'vr/query-replace))

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

(general-define-key
 :states '(normal visual emacs)
 :prefix "["
 "SPC" 'ryche/insert-line-above)

(general-define-key
 :states '(normal visual emacs)
 :prefix "]"
 "SPC" 'ryche/insert-line-below)

(use-package evil-nerd-commenter
  :after evil
  :config
  (general-define-key
   "s-/" 'evilnc-comment-or-uncomment-lines))

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
  (general-define-key "s-;" 'avy-goto-char-timer)
  (general-define-key "s-l" 'avy-goto-line))

(use-package undo-tree
  :init
  (global-undo-tree-mode 1)
  (general-define-key :keymaps 'undo-tree-map "C-?" nil))

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

(use-package format-all
  :commands format-all-buffer)

(general-define-key "s-=" 'format-all-buffer)

;; Search
(use-package ctrlf
  :config
  (ctrlf-mode +1)
  (general-define-key "s-s" 'ctrlf-forward-default
                      "M-s-s" 'ctrlf-backward-default))

(use-package deadgrep
  :config
  (general-define-key "s-S" 'deadgrep))

(use-package wgrep)

;; Windows and buffers
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode t)

(defun ryche/split-window-below-and-switch ()
  "Split the window horizontally and swith to the new pane."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun ryche/split-window-right-and-switch ()
  "Split the window horizontally and swith to the new pane."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(general-define-key
 "s-1" 'delete-other-windows
 "s-2" 'ryche/split-window-below-and-switch
 "s-3" 'ryche/split-window-right-and-switch
 "s-0" 'delete-window
 "s-w" 'delete-window)

(general-define-key
 "s-}" 'next-buffer
 "s-{" 'previous-buffer)

(use-package ace-window
  :config
  (general-define-key "s-o" 'ace-window))

(use-package winner
  :straight (:type built-in)
  :config
  (winner-mode)
  (general-define-key "s-w" 'winner-undo)
  (general-define-key "s-W" 'winner-redo))

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
  (general-define-key
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

(general-define-key "s-f" nil)
(general-define-key "s-f s-s" 'save-buffer
                    "s-f s" 'save-some-buffers)

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

(general-define-key
 "s-f s-." 'ryche/edit-emacs-config
 "s-f ." 'ryche/reload-emacs-config)

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
        dired-recursive-deletes 'top
        dired-dwim-target t)
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
  :config
  (add-to-list 'super-save-triggers 'ace-window)
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
  ;; (defun flycheck-mypy--find-project-root (_checker)
  ;;   (and buffer-file-name
  ;;        (flycheck--locate-dominating-file-matching
  ;;         (file-name-directory buffer-file-name)
  ;;         (rx-to-string
  ;;          `(: bos (or ,@flycheck-python-mypy-config) eos)
  ;;          t))))

  ;; (flycheck-define-checker python-mypy
  ;;   "Mypy syntax and type checker."
  ;;   :command ("mypy"
  ;;             "--show-column-numbers"
  ;;             (config-file "--config-file" flycheck-python-mypy-config)
  ;;             (option "--cache-dir" flycheck-python-mypy-cache-dir)
  ;;             source-original)
  ;;   :working-directory flycheck-mypy--find-project-root
  ;;   :error-patterns
  ;;   ((error line-start (file-name) ":" line (optional ":" column)
  ;;           ": error:" (message) line-end)
  ;;    (warning line-start (file-name) ":" line (optional ":" column)
  ;;             ": warning:" (message) line-end)
  ;;    (info line-start (file-name) ":" line (optional ":" column)
  ;;          ": note:" (message) line-end))
  ;;   :modes python-mode
  ;;   ;; Ensure the file is saved, to work around
  ;;   ;; https://github.com/python/mypy/issues/4746.
  ;;   :predicate flycheck-buffer-saved-p)

  ;; (add-hook 'lsp-after-initialize-hook
  ;;           (lambda ()
  ;;             (when (derived-mode-p 'python-mode)
  ;;               (flycheck-add-next-checker 'lsp 'python-mypy t)
  ;;               (message "Added flycheck checkers."))))

  (setq flycheck-highlighting-mode nil)

  (defhydra hydra-error (global-map "s-e")
    "Flycheck errors"
    ("n" flycheck-next-error "next")
    ("p" flycheck-previous-error "prev")
    ("h" flycheck-first-error "first")
    ("q" nil "quit")))

;; Autocompletion
(use-package company
  :hook ((prog-mode . company-mode))
  :config
  (setq company-idle-delay 0.3))

;; Terminal
(use-package vterm
  :commands vterm
  :config
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil))))

(general-define-key "s-t" 'vterm)

;; Snippets
(use-package yasnippet
  :commands yas-expand
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

(general-define-key
 :keymaps 'yas-minor-mode-map
 "s-y" 'yas-expand
 "s-Y" 'yas-describe-tables)

(use-package yasnippet-snippets
  :defer t)

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
   "s-." 'lsp-find-definition
   "s-," 'lsp-find-references))

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
  :hook ((before-save . py-isort-before-save))
  :config
  (setq py-isort-options '("-m=3")))

(use-package python-black
  :after python
  :hook (python-mode . python-black-on-save-mode))

(use-package pyimport
  :after python)

(defun ryche/format-python ()
  "Make python buffer pretty."
  (interactive)
  (save-buffer)
  (pyimport-remove-unused)
  (py-isort-buffer)
  (python-black-buffer))

(general-define-key
 :keymaps 'python-mode-map
 "s-=" 'ryche/format-python)

(use-package web-mode
  :mode ("\\.html?\\'" "\\.scss\\'")
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

(use-package coffee-mode
  :mode "\\.coffee\\'")

(use-package racket-mode
  :defer t)

;; Jump to definition everywhere
(use-package dumb-jump
  :commands (xref-find-definitions xref-find-references)
  :config
  (setq dumb-jump-selector 'selectrum)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(general-define-key
 "s-." 'xref-find-definitions
 "s-," 'xref-find-references)

;; Custom iterm package
(use-package iterm
  :straight (:type built-in)
  :load-path "lisp/iterm")

(provide 'init)

;;; init.el ends here
