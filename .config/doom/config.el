;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Eric Milks"
      user-mail-address "emmilks@yahoo.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Fira Code" :size 14 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "DejaVu Sans" :size 14))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type `relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Shells
;; Set default shell to bash for shell, term, ansi-term, vterm
(setq shell-file-name "/bin/bash")

;; Org Mode
(after! org
  (setq org-directory "~/Documents/Org/"
        org-agenda-files '("~/Documents/Org/agenda.org")
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-id-locations-file "~/Documents/Org/.org-id-locations"
        org-startup-with-inline-images t
        org-startup-with-latex-preview t
        org-startup-folded t))


;; Org Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

;; Org Roam v2
(after! org-roam
  (setq org-roam-directory "~/Documents/Org/RoamNotes/"
        org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+roam_ref: \n#+filetags: %^g\n#+setupfile: ../latex-header.org \n#+date:%t\n\n")
           :unnarrowed t)
        ("l" "programming language" plain
           "* Characteristics\n\n- Family: %?\n Inspired by: \n\n* Reference:\n\n"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+roam_ref: \n#+filetags: %^g\n#+setupfile: ../latex-header.org \n#+date:%t\n\n")
           :unnarrowed t)
        ("b" "book notes" plain
           "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+roam_ref: \n#+filetags: %^g\n#+setupfile: ../latex-header.org \n#+date:%t\n\n")
           :unnarrowed t))))

(org-roam-db-autosync-mode)

;; Org Journal
(setq org-journal-dir "~/Documents/Org/Journal/"
      org-journal-date-prefix "* "
      org-journal-time-prefix "** "
      org-journal-date-format "%B %d, %Y (%A)"
      org-journal-file-format "%Y-%m-%d.org")

(setq browse-url-browser-function 'eww-browse-url)
