(setq delete-old-versions -1)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil)
(setq default-fill-column 80)
(setq initial-scratch-message "")
(setq frame-resize-pixelwise t)
(setq make-backup-files nil)
(setq auto-save-default nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(show-paren-mode 1)

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

(electric-pair-mode)

(setq evil-want-keybinding nil)

;; Font
(add-to-list 'default-frame-alist '(font . "SF Mono-12" ))

;; Theme
(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-eighties t))

(global-hl-line-mode +1)

;; Evil
(use-package evil :ensure t
  :config
  (evil-mode 1))

(use-package evil-collection :ensure t
  :config
  (evil-collection-init))

(use-package evil-escape :ensure t
  :config
  (evil-escape-mode))

(use-package evil-nerd-commenter :ensure t)

;; Path management
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Ivy
(use-package ivy :ensure t
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

(use-package counsel :ensure t)

(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode)
  )

(use-package avy
  :ensure t
  :init
  (setq avy-background 1)
  :commands (avy-goto-word-1))

;; Ranger
(use-package ranger 
  :ensure t
  :init
  (setq ranger-show-hidden t))

;; Git
(use-package magit
  :ensure t
  )

(use-package evil-magit :ensure t)

;; Project management
(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode))

(use-package counsel-projectile 
  :ensure t
  :config
  (counsel-projectile-mode))

;; Autocompletion
(use-package company
  :ensure t
  :config
  (global-company-mode t))

(use-package flycheck :ensure t)

;; Python
(use-package elpy
  :ensure t
  :init
  (defun my-python-mode-hook ()
    (general-define-key
     :states '(normal visual emacs)
     :prefix ","
     "g" 'elpy-goto-definition))
  (add-hook 'python-mode-hook 'my-python-mode-hook)
  (elpy-enable)
  (delete `elpy-module-highlight-indentation elpy-modules))

;; Enable Flycheck

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))


(use-package auto-virtualenv
  :ensure t
  :init
  (require 'auto-virtualenv)
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv))

;; Yaml
(use-package yaml-mode :ensure t)

;; Custom functions
(defun switch-to-previous-buffer ()
      (interactive)
      (switch-to-buffer (other-buffer (current-buffer) 1)))

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

;; Custom keybinding
(use-package general
  :ensure t
  :config
  (general-define-key
   :states '(normal visual emacs)
   "/" 'swiper)
  (general-define-key
   :states '(normal visual emacs)
   :prefix "SPC"
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
   "pp" 'counsel-projectile-switch-project
   "pf" 'counsel-projectile
   "pb" 'counsel-projectile-switch-to-buffer
   ;; Help
   "hv" 'counsel-describe-variable
   "hf" 'counsel-describe-function
))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode evil-nerd-commenter which-key use-package ranger general exec-path-from-shell evil-collection elpy counsel-projectile color-theme-sanityinc-tomorrow avy auto-virtualenv))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
