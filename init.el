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
      auto-save-default nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)
(global-hl-line-mode +1)
(electric-pair-mode)

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

;; General
(setq use-package-always-ensure t)

(use-package general
  :config
  (general-create-definer set-leader-keys :prefix "SPC")
  (general-create-definer set-local-leader-keys :prefix ","))

;; Font
(add-to-list 'default-frame-alist '(font . "Monaco-12" ))

;; Theme
(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-eighties t))

;; Evil
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

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

;; Path management
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Switch windows
(use-package ace-window
  :config
  (set-leader-keys
    :states '(normal visual emacs)
    "o" 'ace-window))

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

(use-package counsel)

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

;; Git
(use-package magit)

(use-package evil-magit)

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
(use-package org)

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Autocompletion
(use-package company
  :config
  (global-company-mode t))

(use-package flycheck)

;; Python
(use-package elpy
  :init
  (elpy-enable)
  (add-hook 'python-mode-hook 'oneor0/python-mode-hook)
  (delete `elpy-module-highlight-indentation elpy-modules))

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(use-package auto-virtualenv
  :init
  (require 'auto-virtualenv)
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv))

(use-package py-isort)

;; Yaml
(use-package yaml-mode)

;; Racket
(use-package racket-mode
  :init
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

(defun oneor0/python-mode-hook ()
  (set-local-leader-keys
    :keymaps 'python-mode-map
    :states '(normal visual emacs)
    "g" 'elpy-goto-definition
    "G" 'elpy-goto-definition-other-window
    "d" 'elpy-doc
    "a" 'elpy-goto-assignment
    "r" 'elpy-format-code
    "e" 'elpy-shell-send-buffer
    "ss" 'py-isort-buffer
    "sr" 'oneor0/py-autoflake-buffer
    "tt" 'iterm-pytest
    "tf" 'iterm-pytest-file))

(defun oneor0/py-autoflake-buffer()
  "autoflake --remove-all-unused-imports -i unused_imports.py"
  (interactive)
  (if (executable-find "autoflake")
      (progn
        (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                               (shell-quote-argument (buffer-file-name))))
        (revert-buffer t t t))
    (message "Error: Cannot find autoflake executable.")))

(defun oneor0/racket-mode-hook ()
  (set-local-leader-keys
    :keymaps 'racket-mode-map
    :states '(normal visual emacs)
    "'" 'racket-repl
    "sb" 'racket-run
    "sr" 'racket-send-region))

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
 "/" 'swiper)

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
 "." 'edit-emacs-config
 "/" 'counsel-ag
 "TAB" 'mode-line-other-buffer
 "SPC" 'counsel-switch-buffer
 ;; Apps
 "ar" 'ranger
 "ad" 'deer
 ;; Quit
 "qq" 'save-buffers-kill-emacs
 ;; Files
 "fs" 'save-buffer
 "ff" 'counsel-find-file
 ;; Buffers
 "bd" 'kill-current-buffer
 "bk" 'kill-buffer
 "bb" 'counsel-switch-buffer
 ;; Search
 "ss" 'swiper-thing-at-point
 "sp" 'counsel-ag-thing-at-point
 "sr" 'ivy-resume
 ;; Jump
 "jl" 'avy-goto-line
 "jf" 'avy-goto-char-timer
 "jr" 'avy-resume
 ;; Git
 "gs" 'magit-status
 "gb" 'magit-blame
 ;; Window
 "wl" 'windmove-right
 "wh" 'windmove-left
 "wk" 'windmove-up
 "wj" 'windmove-down
 "w/" 'split-window-right
 "w-" 'split-window-below
 "wd" 'delete-window
 ;; Projects
 "pp" 'counsel-projectile-switch-project
 "pf" 'counsel-projectile
 "pb" 'counsel-projectile-switch-to-buffer
 ;; Help
 "hv" 'counsel-describe-variable
 "hf" 'counsel-describe-function
)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-bullets flycheck org-mode evil-magit magit ace-window evil-escape py-autoflake py-isort racket-mode evil-unimpaired evil-surround yaml-mode evil-nerd-commenter which-key use-package ranger general exec-path-from-shell evil-collection elpy counsel-projectile color-theme-sanityinc-tomorrow avy auto-virtualenv))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
