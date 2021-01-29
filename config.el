;;; $$DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ian Wahbe"
      user-mail-address "ian@wahbe.com")

(setq fancy-splash-image (concat doom-private-dir "doom3small.png"))
;; NOTE: The demon is named Deborah (ie Debra)

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
(setq doom-font (font-spec :family "Fira Code" :size 12))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-vibrant)
(setq doom-theme 'doom-moonlight)


(defun =name-to-emacs-file (name)
  "Convert a name to a appropriate file name."
  (shell-quote-argument
   (string-replace " " "-" (string-replace "_" "-" (downcase name)))))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(defun =org-capture-add-class (name tag hotkey)
  "Sets up a class for org capture."
  (add-to-list 'org-capture-templates
               `(,(concat "sc" hotkey) ,name entry (file+olp+datetree ,(concat org-directory (=name-to-emacs-file name) ".org"))
                 ,(concat "* CLASS %t :SCHOOL:" tag ":\n%T\n%?"))))

(defun =org-capture-setup-school ()
  (add-to-list 'org-capture-templates '("s" "School"))
  (add-to-list 'org-capture-templates '("sc" "School classes"))
  (=org-capture-add-class "Computability and Complexity" "CSCI387" "c")
  (=org-capture-add-class "Topics in Systems" "CSCI442" "s")
  (=org-capture-add-class "Parallelism and Concurrency" "CSCI361" "p")
  (=org-capture-add-class "Thesis" "CSCI470" "t")
  (add-to-list 'org-capture-templates `("sn" "School Notes" entry (file+olp+datetree ,(concat org-directory "school.org"))
                                        "* NOTE %? :SCHOOL:\n%T"))
  (add-to-list 'org-capture-templates `("sm" "School Meeting" entry (file+olp+datetree ,(concat org-directory "school.org"))
                                        "* MEETING with %? :SCHOOL:MEETING:\n%T"))
  (add-to-list 'org-capture-templates `("st" "School Assignment" entry (file+olp+datetree ,(concat org-directory "school.org"))
                                        "* TODO %? :SCHOOL:\n%T")))

(after! org
  (=org-capture-setup-school)
  (setq org-agenda-span 20))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; For absolute line numbers, set this to `t'.
(setq display-line-numbers-type 'relative)

;; This changes how modes activate.
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

;; Customize key bindings
(use-package! company
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1 ; this makes company respond in real time (no delay)
        company-dabbrev-downcase 1
        company-require-match 'never)
  (global-company-mode t)

  :bind (:map company-active-map
  	 ("<return>" . nil)
	 ("RET" . nil)
         ("<tab>" . nil)
         ("TAB" . nil)
  	 ("C-@" . #'company-complete-selection) ;also means space
	 ("C-SPC" . #'company-complete-selection)
	 ("C-<space>" . #'company-complete-selection)
	 ("M-p" . #'company-select-previous-or-abort)
	 ("M-n" . #'company-select-next-or-abort)))

(after! (tree-sitter)
  (add-to-list 'tree-sitter-load-path
               (concat user-emacs-directory ".local/straight/repos/"
                       "emacs-tree-sitter/tree-sitter-langs/" "bin/")))


(setq lsp-rust-server 'rust-analyzer)

(let ((dicpath (expand-file-name "~/.dictionaries")))
  (unless (file-exists-p dicpath)
    (async-shell-command (concat "git clone git://anongit.freedesktop.org/libreoffice/dictionaries " dicpath)))
	(setenv "DICPATH" (concat dicpath "/en")))

;; To match the performance of modern editors, Emacs needs to consume a similar amount of resources.
(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024)) ;; 1mb

;; Make scroll-other-window consistant. This remaps isearch-regex-backward.
(map! :g "C-M-r" #'scroll-other-window-down
      :n "g b" #'better-jumper-jump-backward)

(load! "transliterate.el")
(load! "evil-structured.el")

(setq writeroom-fullscreen-effect nil)

(defun =python-expand-args (arg-list)
  (mapconcat (lambda (s)
               (let* ((pre-default (car (split-string s "=" t "[[:blank:]]")))
                      (untyped (car (split-string pre-default ":" t "[[:blank:]]"))))
               (concat "self." pre-default " = " untyped)))
             (seq-filter (lambda (s) (not (or (equal "*" s) (equal "self" s))))
                         (split-string arg-list "," t "[[:blank:]]"))
             "\n"))

(use-package! olivetti
  :config
  (setq-default olivetti-body-width 90))
(map! :after olivetti :map doom-leader-toggle-map :desc "Olivetti mode" "o" #'olivetti-mode)

(defun =project-scratch-buffer-dwim (&optional arg same-window-p)
  "Pop up a project scratch buffer if none exists, go to one if
it's already there. If already in the buffer, close it."
  (interactive "P")
  (if (string-match-p "^\\*doom:scratch" (buffer-name))
      (if (= 1 (length (get-buffer-window-list (buffer-name))))
          (progn (kill-current-buffer) (message "Killed buffer"))
        (progn (set-window-buffer (selected-window) (other-buffer (current-buffer)))
               (message "Switched buffer"))
        )
    (doom/open-project-scratch-buffer arg same-window-p)))


(defun =switch-to-project-scratch-buffer-dwim (&optional arg)
  "Just like doom/project-scratch-buffer-dwim, except the scratch
buffer occupies the current window if it exists."
  (interactive "P")
  (=project-scratch-buffer-dwim arg 'same-window))

(defmacro =dbg(form)
  "Prints FORM => res where res is what FORM evaluates to. Returns res."
  `(let ((res ,form)) (message "dbg: %s => %s" '(,@form) res) res))

(defmacro =switch (on &rest conditionals)
  "A switch statment.
:comp specifies the 2-arity function to use for comparison. Defaults to `equal'.
Specify a default argument with `t'. Note: this macro desugars into a `cond' statment."
  (declare (indent defun))
  (let* ((comp-provided (equal (car conditionals) :comp))
         (comp (if comp-provided (cadr conditionals) 'equal))
         (conditions (if comp-provided (cddr conditionals) conditionals)))
    `(let ((on ,on))(cond ,@(mapcar (lambda (x)
                                      (if (equal (car x) t)
                                          `(t ,(cadr x))
                                        (list `(funcall (quote ,comp) ,(car x) ,on) (cadr x))))
                                    conditions)))))


(map! :map doom-leader-project-map :desc "(dwim) Pop up scratch buffer" "x"
      #'=project-scratch-buffer-dwim)
(map! :map doom-leader-project-map :desc "(dwim) Switch to scratch buffer" "X"
      #'=switch-to-project-scratch-buffer-dwim)

(load! "confirm-init.el")
(=check-doom!-init)

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
