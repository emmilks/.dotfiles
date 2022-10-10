(setq gc-cons-threshold (* 50 1000 1000))

;; Use-Package Packages
(setq package-archives
  '(("melpa" . "https://melpa.org/packages/")
    ("elpa" . "https://elpa.gnu.org/packages/")
    ("org"   . "https://orgmode.org/elpa/")))

;;; BOOTSTRAP USE-PACKAGE
(package-initialize)
(setq use-package-always-ensure t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; Prevent main screen from appearing at startup
(setq inhibit-startup-message t)
;; Name and email
(setq user-full-name "Eric Milks"
    user-mail-address "emmilks@yahoo.com")

;; Line Numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
        term-mode-hook
        shell-mode-hook
        eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0 ))))
;; Change yes-or-no to y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)
;; Disable bell
(setq ring-bell-function 'ignore)
;; Disable unneeded UI elements
;;(menu-bar-mode -1) Uncomment when you are comfortable with emacs
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Cleanup whitespace
(add-hook 'before-save-hook 'whitespace-cleanup)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq user-emacs-directory "~/.config/emacs/")
(use-package no-littering)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(global-auto-revert-mode t)

(setq-default tab-width 4
      indent-tabs-mode nil)

(defun config-visit ()
  (interactive)
  (find-file "~/.config/emacs/config.org"))
(global-set-key (kbd "C-c e") 'config-visit)

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(global-set-key (kbd "<s-return>") 'ansi-term)

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python :results output"))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))

(setq org-confirm-babel-evaluate nil)

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(use-package diminish)

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(defun kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-x b") 'ibuffer)

(use-package ivy)

(use-package async
  :config
  (dired-async-mode 1))

(use-package popup)

(use-package helm
  :init
  (helm-mode 1)
  :bind
  ("M-x" . 'helm-M-x)
  ("C-x C-f" . 'helm-find-files)
  ("C-x C-b" . 'helm-buffers-list)
  :config
  ;;(require 'helm-config)
  (setq helm-autoresize-max-height 0
    helm-autoresize-min-height 40
    helm-M-x-fuzzy-match t
    helm-buffers-fuzzy-matching t
    helm-recentf-fuzzy-match t
    helm-semantic-fuzzy-match t
    helm-imenu-fuzzy-match t
    helm-split-window-in-side-p nil
    helm-move-to-line-cycle-in-source nil
    helm-ff-search-library-in-sexp t
    helm-scroll-amount 8
    helm-echo-input-in-header-line t)
  (helm-autoresize-mode 1)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ;rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)) ; make TAB work in terminal

(use-package helm-projectile
  :after projectile
  :config
  (helm-projectile-on))

(defvar em/default-font-size 115)
(defvar em/default-variable-font-size 115)

(set-face-attribute 'default nil :font "Fira Code Retina" :height em/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height em/default-font-size)
(set-face-attribute 'variable-pitch nil :font "Liberation Serif" :height em/default-variable-font-size :weight 'regular)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;;(doom-themes-neotree-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package ess)

(use-package projectile
  :init
  (projectile-mode 1)
  :config
  ;; let projectile call make
  (global-set-key (kbd "<f5>") 'projectile-compile-project))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 5)
              (projects . 5))))

(use-package company
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  :hook
  (prog-mode . company-mode))

(setq gc-cons-threshold (* 2 1000 1000))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company dashboard ess doom-modeline all-the-icons doom-themes helm-projectile helm popup ivy which-key rainbow-delimiters rainbow-mode diminish org-auto-tangle no-littering use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
