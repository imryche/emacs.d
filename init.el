;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration

;;; Code:

;; Garbage collection
(defconst original-gc-cons-threshold gc-cons-threshold
  "Initial value of garbage collection threshold.")
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook (lambda ()(setq gc-cons-threshold original-gc-cons-threshold)))

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
      cursor-type 'box
      fill-column 80
      scroll-margin 5
      initial-scratch-message ""
      frame-resize-pixelwise t
      make-backup-files nil
      create-lockfiles nil
      auto-save-default nil
      use-dialog-box nil
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

;; Profile startup time
(use-package esup
  :init
  (setq esup-depth 0)
  :commands (esup))

;; UI
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-13" ))
(when (string= system-type "darwin")
  (add-to-list 'default-frame-alist '(font . "JetBrains Mono-14" )))

(use-package autothemer)
(load-theme 'nohl t)

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
  (setq evil-insert-state-cursor 'box)
  :config
  (evil-mode 1)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-leader 'normal (kbd "SPC") t)
  (defvar leader-map (make-sparse-keymap)
    "Keymap for \"leader key\" shortcuts.")

  ;; binding "SPC" to the keymap
  (define-key evil-normal-state-map (kbd "SPC") leader-map)
  (define-key evil-visual-state-map (kbd "SPC") leader-map)

  (with-eval-after-load 'evil-maps
    (define-key evil-normal-state-map (kbd "C-n") nil)
    (define-key evil-normal-state-map (kbd "C-p") nil)))

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package visual-regexp)
(define-key leader-map "r" 'vr/query-replace)

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

(define-key evil-normal-state-map (kbd "[SPC") 'ryche/insert-line-above)
(define-key evil-normal-state-map (kbd "]SPC") 'ryche/insert-line-above)

(use-package evil-nerd-commenter
  :after evil)
(define-key evil-normal-state-map (kbd "C-'") 'evilnc-comment-or-uncomment-lines)

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

(use-package expand-region)
(define-key leader-map "e" 'er/expand-region)

(use-package avy)
(define-key leader-map "j" 'avy-goto-char-timer)
(define-key leader-map "l" 'avy-goto-line)

(use-package format-all
  :commands format-all-buffer)
(define-key evil-normal-state-map (kbd "C-=") 'format-all-buffer)

(use-package deadgrep
  :commands deadgrep)
(define-key leader-map "f" 'deadgrep)

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

(define-key leader-map "1" 'delete-other-windows)
(define-key leader-map "2" 'ryche/split-window-below-and-switch)
(define-key leader-map "3" 'ryche/split-window-right-and-switch)
(define-key leader-map "w" 'delete-window)

(define-key leader-map "q" 'kill-this-buffer)

(use-package ace-window)
(define-key leader-map (kbd "TAB") 'ace-window)

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

(define-key leader-map "i" 'ibuffer)
(define-key leader-map "b" 'consult-buffer)
(define-key leader-map "B" 'consult-buffer-other-window)

(use-package marginalia
  :init
  (marginalia-mode))

;; Isearch
(setq search-whitespace-regexp ".*?")

;; File management
(recentf-mode 1)
(setq recentf-max-menu-items 25
      recentf-max-saved-items 25)

(setq vc-follow-symlinks t) ;; Follow symlinks without asking

(define-key leader-map "s" 'save-buffer)
(define-key leader-map "S" 'save-some-buffers)

(defun ryche/edit-emacs-config ()
  "Open Emacs configuration file."
  (interactive)
  (find-file user-init-file))

(define-key evil-normal-state-map (kbd "/") 'find-file)

(define-key leader-map "." 'ryche/edit-emacs-config)

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

(define-key leader-map "d" 'dired-jump)

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

(define-key leader-map "cs" 'magit-status)

(use-package magit-todos
  :defer t)

(use-package git-link
  :commands git-link
  :config
  (setq git-link-open-in-browser t))

(define-key leader-map "cl" 'git-link)

(use-package git-timemachine
  :commands git-timemachine
  :config
  (evil-define-minor-mode-key 'normal 'git-timemachine-mode
    "gp" 'git-timemachine-show-previous-revision
    "gn" 'git-timemachine-show-next-revision))

(define-key leader-map "ct" 'git-timemachine)

(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (setq projectile-enable-caching nil))
(define-key leader-map "p" 'projectile-find-file)
(define-key leader-map "P" 'projectile-switch-project)

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
  (setq flycheck-highlighting-style nil))
(define-key evil-normal-state-map (kbd "C-.") 'flycheck-next-error)
(define-key evil-normal-state-map (kbd "C-,") 'flycheck-previous-error)

;; Autocompletion
(use-package company
  :hook ((prog-mode . company-mode))
  :config
  (company-tng-mode)
  (setq company-idle-delay 0.3)
  (delete 'company-clang company-backends))

(use-package python-mode
  :defer t)

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

(evil-define-key 'normal python-mode-map (kbd "C-=") 'ryche/format-python)

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

(use-package json-mode
  :mode ("\\.json\\'"))

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

(define-key leader-map "g" 'xref-find-definitions)
(define-key leader-map "G" 'xref-find-references)

;; Custom iterm package
(use-package iterm
  :ensure nil
  :load-path "lisp/iterm")

(provide 'init)

;;; init.el ends here
