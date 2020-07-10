;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ian Wahbe"
      user-mail-address "ian@wahbe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; This changes how modes activate.
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

;; Customize key bindings
(use-package! company
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.1) ; this makes company respond in real time (no delay)
  (setq company-dabbrev-downcase 1)
  (setq company-require-match 'never)
  (global-company-mode t)

  :bind (:map company-active-map
  	     ("<return>" . nil)
	       ("RET" . nil)
  	     ("C-@" . #'company-complete-selection) ;also means space
	       ("C-SPC" . #'company-complete-selection)
	       ("C-<space>" . #'company-complete-selection)
	       ("M-p" . #'company-select-previous-or-abort)
	       ("M-n" . #'company-select-next-or-abort))
  )
;; TODO: search for text in project, probably bind to SPC p S or rebind SPC p s
;; TODO: setup lsp-mdoe for typescript (and rust and c/cpp and everything else)
;; TODO: figure out if spelling works

;; To match the performance of modern editors, Emacs needs to consume a similar amount of resources.
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((projectile-project-test-cmd . "../test.sh")
     (projectile-project-compilation-cmd . "../build.sh"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
