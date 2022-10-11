(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

(use-package diminish
  :straight t)

(use-package gcmh
  :straight t
  :diminish gcmh-mode
  :custom
  (gcmh-mode 1)
  (gcmh-idle-delay 5)
  (gcmh-high-cons-threshold (* 256 1024 1024))
  (gc-cons-percentage 0.2))

(defvar em/default-font-size 115)
(defvar em/default-variable-font-size 115)

(use-package emacs
  :after doom-themes
  :init
  (set-face-attribute 'default nil :font "Fira Code" :height em/default-font-size :weight 'regular)
  (set-face-attribute 'fixed-pitch nil :font "Fira Code" :height em/default-font-size :weight 'regular)
  (set-face-attribute 'variable-pitch nil :font "Liberation Serif" :height em/default-variable-font-size :weight 'regular)
  :config
  ;; Prevent main screen from appearing at startup
  (setq inhibit-startup-message t)
  ;; Line Numbers
  (column-number-mode)
  (global-display-line-numbers-mode t)

  ;; Disable line numbers for some modes
  (dolist (mode '(term-mode-hook
        shell-mode-hook
        eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0 ))))
  (setq use-short-answers t)
  (show-paren-mode t)
  ;; Disable bell
  (setq ring-bell-function 'ignore)
  (setq auto-save-file-name-transforms
    `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  ;; Text Encode
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (global-auto-revert-mode t)    ;; Auto Revert Buffer
  (setq-default tab-width 4    ;; Tab behavior
    indent-tabs-mode nil)
  (delete-selection-mode t)    ;; Start writing after deletion
  (setq-default fill-column 80)
  (setq-default frame-title-format '("%b")) ; make window title the buffer name
  ;; Increase the amount of data Emacs reads from the process
  (setq read-process-output-max (* 1024 1024))
  :hook
  (before-save-hook . whitespace-cleanup))

(setq user-emacs-directory "~/.config/emacs/")
(use-package no-littering
  :straight t)

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
  :straight t
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(use-package diminish
    :straight t)

(use-package rainbow-mode
  :straight t
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :straight t
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

(use-package async
  :straight t
  :config
  (dired-async-mode 1))

(use-package doom-themes
    :straight t
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
    :straight t
    :if (display-graphic-p))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

(use-package ess
    :straight t)

(use-package projectile
  :straight t
  :init
  (projectile-mode 1)
  :config
  ;; let projectile call make
  (global-set-key (kbd "<f5>") 'projectile-compile-project))

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 5)
              (projects . 5))))

(use-package vertico
  :straight t
  :init
  (vertico-mode))

(use-package savehist
  :straight t
  :init
  (savehist-mode))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :after vertico
  :straight t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package company
  :straight t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  :hook
  (prog-mode . company-mode))
