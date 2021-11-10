;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration

;;; Code:

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
(require 'use-package-ensure)

(setq use-package-always-ensure t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Garbage collection
(setq gc-cons-threshold 100000000)

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
(blink-cursor-mode 0)

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

;; To use Ctrl+hjkl as arrow keys system-wide, I need to remap help keybindings
(general-define-key "s-?" help-map)

;; Profile startup time
(use-package esup
  :init
  (setq esup-depth 0)
  :commands (esup))

;; UI
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-14" ))

(use-package color-theme-sanityinc-tomorrow
  :config
  (color-theme-sanityinc-tomorrow-night))

(use-package hl-line
  :config
  (global-hl-line-mode))

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

(electric-pair-mode 1)

(use-package undo-fu)

(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-fu)
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

(use-package move-text
  :config
  (general-define-key
   "M-k" 'move-text-up
   "M-j" 'move-text-down))

(use-package visual-regexp
  :config
  (general-define-key "s-t" 'vr/query-replace))

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

(general-define-key "s-'" 'comment-line)

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
   "s-a" 'er/expand-region
   "s-A" 'er/contract-region))

(use-package avy
  :config
  (general-define-key "s-;" 'avy-goto-char-timer)
  (general-define-key "s-l" 'avy-goto-line))

(use-package format-all
  :commands format-all-buffer)
(general-define-key "s-=" 'format-all-buffer)

(use-package deadgrep
  :commands deadgrep)
(general-define-key "M-F" 'deadgrep)

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

(general-define-key
 "s-]" 'evil-jump-forward
 "s-[" 'evil-jump-backward)

(use-package ace-window
  :config
  (general-define-key "s-o" 'ace-window))

;; (use-package ibuffer
;;   :commands (ibuffer)
;;   :config
;;   (general-define-key
;;    "C-x C-b" 'ibuffer))

;; (use-package ibuffer-vc
;;   :after ibuffer
;;   :config
;;   (add-hook 'ibuffer-hook 'ibuffer-vc-set-filter-groups-by-vc-root))

;; Completion system
(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :init
  (savehist-mode))

(use-package consult
  :commands (consult-buffer consult-buffer-other-window consult-line consult-ripgrep)
  :init
  (setq consult-project-root-function #'projectile-project-root)
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))
(general-define-key
 "s-b" 'consult-buffer
 "s-B" 'consult-buffer-other-window)
(general-define-key
 :states '(normal visual emacs)
 "M-f" 'consult-ripgrep
 "s-L" 'consult-goto-line)

(use-package marginalia
  :init
  (marginalia-mode))

;; (use-package embark
;;   :config
;;   (general-define-key "s-x" 'embark-act
;;                       "s-X" 'embark-dwim))

;; (use-package embark-consult
;;   :after (embark consult))
;; Isearch
(general-define-key
 :states '(normal visual emacs)
 "s-f" 'isearch-forward
 "s-F" 'isearch-forward-symbol-at-point
 "s-r" 'isearch-backward)
(general-define-key
 :keymaps 'isearch-mode-map
 "s-f" 'isearch-repeat-forward
 "s-r" 'isearch-repeat-backward)

;; File management
(recentf-mode 1)
(setq recentf-max-menu-items 25
      recentf-max-saved-items 25)

(setq vc-follow-symlinks t) ;; Follow symlinks without asking

(general-define-key "s-s" 'save-buffer
                    "s-S" 'save-some-buffers)

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
 :states '(normal visual emacs)
 "/" 'find-file)

(general-define-key
 "s-/ ." 'ryche/edit-emacs-config
 "s-/ s-." 'ryche/reload-emacs-config)

(use-package dired
  :ensure nil
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
  (super-save-mode +1))

(use-package saveplace
  :hook ((after-init . save-place-mode)))

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

;; Git
(use-package magit
  :commands (magit-status)
  :config
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
  :commands git-timemachine
  :config
  (evil-define-minor-mode-key 'normal 'git-timemachine-mode
    "gp" 'git-timemachine-show-previous-revision
    "gn" 'git-timemachine-show-next-revision))

(general-define-key "s-g t" 'git-timemachine)

;; (use-package diff-hl
;;   :init
;;   (global-diff-hl-mode)
;;   :config
;;   (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
;;   (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package projectile
  :config
  (projectile-mode +1)
  (general-define-key "s-p" 'projectile-find-file
                      "s-P" 'projectile-switch-project
;; Writing
;; (use-package darkroom
;;   :commands darkroom-mode)
                      "s-T" 'projectile-replace))

(use-package org
  :defer t
  :init
  (setq org-log-done 'time
        org-directory "~/Dropbox/org"
        org-agenda-files (list "~/Dropbox/org/projects/")
        org-link-frame-setup '((file . find-file)))
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

(use-package visual-fill-column
  :config
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

;; Syntax checking
(use-package flycheck
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-highlighting-style nil)
  (general-define-key "s-e" 'flycheck-next-error
                      "s-E" 'flycheck-previous-error))

;; Autocompletion
(use-package company
  :hook ((prog-mode . company-mode))
  :config
  (company-tng-mode)
  (setq company-idle-delay 0.3))

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

;; ;; Languages
;; (use-package lsp-mode
;;   :hook ((python-mode . lsp)
;;          (ruby-mode . lsp)
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :config
;;   (setq lsp-headerline-breadcrumb-enable nil
;;         lsp-enable-symbol-highlighting nil
;;         lsp-lens-enable nil
;;         lsp-semantic-tokens-enable nil
;;         lsp-eldoc-enable-hover nil
;;         lsp-modeline-diagnostics-enable nil
;;         lsp-signature-render-documentation nil
;;         lsp-diagnostic-package :none)
;;   (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.mypy_cache\\'")
;;   (general-define-key
;;    :keymaps 'python-mode-map
;;    "s-." 'lsp-find-definition
;;    "s-," 'lsp-find-references))

;; (use-package lsp-pyright
;;   :defer t
;;   :hook
;;   (python-mode . (lambda ()
;;                    (require 'lsp-pyright)
;;                    (lsp-deferred)))
;;   :config
;;   (setq lsp-pyright-typechecking-mode "off"))

(use-package auto-virtualenv
  :defer t
  :init
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

(defun ryche/format-python ()
  "Make python buffer pretty."
  (interactive)
  (save-buffer)
  (py-isort-buffer)
  (python-black-buffer))

(general-define-key
 :keymaps 'python-mode-map
 "s-=" 'ryche/format-python)

(use-package web-mode
  :mode ("\\.html?\\'" "\\.scss\\'" "\\.hbs\\'" "\\.handlebars\\'")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-expanding t))

(use-package js2-mode
  :mode ("\\.js\\'")
  :config
  (setq js-indent-level 2
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil))

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
  :config
  (setq dumb-jump-selector 'selectrum)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(general-define-key
 "s-." 'xref-find-definitions
 "s-," 'xref-find-references)

;; Custom iterm package
(use-package iterm
  :ensure nil
  :load-path "lisp/iterm")

(provide 'init)

;;; init.el ends here
