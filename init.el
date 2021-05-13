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
(electric-pair-mode)

(setq default-directory (concat (getenv "HOME") "/"))

;; Silence compiler warnings as they can be pretty disruptive
(setq comp-async-report-warnings-errors nil)

;; Garbage collection
(setq gc-cons-threshold 20000000)
(add-function :after after-focus-change-function #'garbage-collect)

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq byte-compile-warnings '(cl-functions))
(setq-default indent-tabs-mode nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

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

(setq default-directory (expand-file-name "~/"))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Always compile packages and use the newest version available
(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

(use-package general)
(general-create-definer set-local-leader-keys :prefix ",")
(general-create-definer set-leader-keys :prefix "SPC")

(use-package esup
  :init
  (setq esup-depth 0)
  :ensure t
  :pin melpa
  :commands (esup))

(use-package hl-line
  :hook ((after-init . global-hl-line-mode)))

(use-package saveplace
  :hook ((after-init . save-place-mode)))

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
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-major-mode-icon nil
        doom-modeline-buffer-modification-icon nil))

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package minions
  :config
  (setq minions-mode-line-lighter ""
        minions-mode-line-delimiters '("" . ""))
  (minions-mode 1))

(use-package evil
  :defer .1
  :init
  (setq evil-want-keybinding nil
        evil-undo-system 'undo-tree)
  (global-undo-tree-mode)
  :config
  (evil-mode 1)
  (with-eval-after-load 'evil-maps
    (define-key evil-normal-state-map (kbd "C-n") nil)
    (define-key evil-normal-state-map (kbd "C-p") nil)))

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
  :after evil
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

(use-package selectrum
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
  (set-leader-keys
    :states '(normal visual emacs)
    "/" 'consult-ripgrep
    "SPC" 'consult-buffer))

(use-package marginalia
  :init
  (marginalia-mode))

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

(use-package ibuffer
  :init
  (set-leader-keys
    :states '(normal visual emacs)
    "bb" 'ibuffer))

(use-package ibuffer-vc
  :config
  (add-hook 'ibuffer-hook 'ibuffer-vc-set-filter-groups-by-vc-root))

(use-package magit
  :config
  (setq magit-completing-read-function #'selectrum-completing-read)
  (set-leader-keys
    :states '(normal visual emacs)
    "gs" 'magit-status
    "gb" 'magit-blame)
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

(use-package git-timemachine
  :defer t
  :init
  (set-leader-keys
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
  (set-leader-keys
    :states '(normal visual emacs)
    "pp" 'projectile-switch-project
    "pf" 'projectile-find-file
    "pr" 'projectile-replace
    "pR" 'projectile-replace-regexp))

(use-package format-all
  :init
  (set-leader-keys
    :states '(normal visual emacs)
    "=" 'format-all-buffer))

(use-package whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode t))

(use-package writeroom-mode)

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
  :hook ((prog-mode . company-mode))
  :config
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 1))

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

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-highlighting-mode nil)
  (set-leader-keys
    :states '(normal visual emacs)
    "en" 'flycheck-next-error
    "ep" 'flycheck-previous-error
    "el" 'flycheck-list-errors))

(use-package lsp-mode
  :hook (
         (python-mode . lsp)
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
  (set-leader-keys
    :states '(normal visual emacs)
    :keymaps 'python-mode-map
    "." 'lsp-find-definition
    "," 'lsp-find-references))

(use-package lsp-pyright
  :defer t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(use-package auto-virtualenv
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
  (add-hook 'window-configuration-change-hook 'auto-virtualenv-set-virtualenv))

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

(use-package pyimport)

(use-package web-mode
  :mode "\\.html?\\'"
  :config
  (add-hook 'web-mode-hook (function (lambda () (setq evil-shift-width 2))))
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-expanding t))

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

(use-package iterm
  :load-path "lisp/iterm")

(set-local-leader-keys
  :keymaps 'python-mode-map
  :states '(normal visual emacs)
  "t" 'iterm-pytest
  "T" 'iterm-pytest-file)

;; Custom functions
(defun ryche/edit-emacs-config ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun ivy-with-thing-at-point (cmd)
  (let ((ivy-initial-inputs-alist
         (list
          (cons cmd (thing-at-point 'symbol)))))
    (funcall cmd)))

(defun ryche/counsel-rg-thing-at-point ()
  (interactive)
  (ivy-with-thing-at-point 'counsel-rg))

(defun swiper-thing-at-point ()
  (interactive)
  (ivy-with-thing-at-point 'swiper))

(defun ryche/split-right-switch ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun ryche/split-below-switch ()
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
 :prefix "["
 "SPC" 'insert-line-above)

(general-define-key
 :states '(normal visual emacs)
 :prefix "]"
 "SPC" 'insert-line-below)

(set-leader-keys
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
