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

(defun =org-capture-add-class (name tag hotkey &optional dir)
  "Sets up a class for org capture. Class has an associated CLASS and TODO task.
NAME is used to generate a file with `=name-to-emacs-file'. TAG is the associated org-mode tag.
HOTKEY is the hotkey in `org-capture-templates' after `sc'. It must be unique ignoring capitalization."
  (let* ((path (expand-file-name (concat (=name-to-emacs-file name) ".org")
                                 (or dir org-directory)))
         (capture-body `(entry (file+olp+datetree ,path)))
         (tag (upcase tag)))
    (when dir (add-to-list 'org-agenda-files path))
    (add-to-list 'org-capture-templates ;; Add a new class meeting
                 `(,(concat "sc" (downcase hotkey)) ,(concat "CLASS " name) ,@capture-body
                   ,(concat "* CLASS %? :SCHOOL:" tag ":\n%T\n") :jump-to-captured t))
    (add-to-list 'org-capture-templates ;; Add a new class todo
                 `(,(concat "sc" (upcase hotkey)) ,(concat "TODO " name) ,@capture-body
                   ,(concat "* TODO %? :SCHOOL:" tag ":\n%T\n")))))

(defun =org-capture-setup-school ()
  "Modify `org-capture-templates' to accommodate classes."
  (add-to-list 'org-capture-templates '("s" "School"))
  (add-to-list 'org-capture-templates '("sc" "School classes"))
  (=org-capture-add-class "Computability and Complexity" "CSCI387" "c" "~/Projects/cs387-comp-comp")
  (=org-capture-add-class "Topics in Systems" "CSCI442" "s")
  (=org-capture-add-class "Parallelism and Concurrency" "CSCI361" "p" "~/Projects/cs361-parallelism-and-concurrency")
  (=org-capture-add-class "Thesis" "CSCI470" "t" "~/Projects/thesis")
  (=org-capture-add-class "CS221 Tutoring" "CSCI221TA" "g")
  (add-to-list 'org-capture-templates `("sn" "School Notes" entry (file+olp+datetree ,(concat org-directory "school.org"))
                                        "* NOTE %? :SCHOOL:\n%T"))
  (add-to-list 'org-capture-templates `("sm" "School Meeting" entry (file+olp+datetree ,(concat org-directory "school.org"))
                                        "* MEETING with %? :SCHOOL:MEETING:\n%T"))
  (add-to-list 'org-capture-templates `("st" "School TODO" entry (file+olp+datetree ,(concat org-directory "school.org"))
                                        "* TODO %? :SCHOOL:\n%T")))

(after! org
  (=org-capture-setup-school)
  (setq org-agenda-span 20
        org-edit-src-content-indentation 0
        org-src-preserve-indentation nil))

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
        company-idle-delay 0.1
        company-dabbrev-downcase 1
        company-require-match 'never)
  ;; expiremental
  (setq company-ispell-cache-results t)
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

(use-package! lsp-mode
  :init
  (setq lsp-rust-server 'rust-analyzer
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-metals-show-inferred-type t))

;; To match the performance of modern editors, Emacs needs to consume a similar amount of resources.
(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024)) ;; 1mb

;; Make scroll-other-window consistant. This remaps isearch-regex-backward.
(map! :g "C-M-r" #'scroll-other-window-down
      :n "g b" #'better-jumper-jump-backward)

(setq writeroom-fullscreen-effect nil)

(use-package! sqlup-mode
  :hook (sql-mode sql-interactive-mode))

(defun =python-init-expand-args (arg-list)
  "Converts the argument list for a python init function into an assignment
x, y, z: T gets converted to
self.x = x
self.y = y
self.z: T = z
"
  (mapconcat
   (lambda (s)
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
               (message "Switched buffer")))

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
:comp specifies the 2-arity function to use for comparison.
Defaults to `equal'. Specify a default argument with `t'. Specify
a binding import statment with `@'. `@' will be bound to the `on'
argument. Note: this macro desugars into a `cond' statment."
  (declare (indent defun))
  (let* ((comp-provided (equal (car conditionals) :comp))
         (comp (if comp-provided (cadr conditionals) 'equal))
         (conditions (if comp-provided (cddr conditionals) conditionals))
         (switch-case (lambda (x)
                        (if (equal (car x) t)
                            `(t ,(cadr x))
                          (if (equal (car x ) '@)
                              `(t (let ((@ on)) ,(cadr x)))
                            (list `(funcall (quote ,comp) ,(car x) on) (cadr x)))))))
    `(let ((on ,on)) ;; To ensure that on is only computed once at the beginning.
       (cond ,@(mapcar switch-case
                       conditions)))))


(map! :map doom-leader-project-map :desc "(dwim) Pop up scratch buffer" "x"
      #'=project-scratch-buffer-dwim)
(map! :map doom-leader-project-map :desc "(dwim) Switch to scratch buffer" "X"
      #'=switch-to-project-scratch-buffer-dwim)
(map! :leader :desc "Expand region" "e"
      #'er/expand-region)

;; Broken
(setq-hook! 'gfm-mode-hook +format-with :none)
(setq-hook! 'markdown-mode-hook +format-with :none)
(add-to-list '+format-on-save-enabled-modes 'markdown-mode t)


(load! "confirm-init.el")
(=check-doom!-init)

(use-package! vterm
  :init
  (defun =vterm-extend-source-path ()
    "The path to the file containing vterm enabled extensions."
    (let ((extension (cond
                      ((equal vterm-shell "/bin/zsh") "-zsh.sh")
                      ((equal vterm-shell "/bin/bash") "-bash.sh")
                      ((equal vterm-shell "/bin/fish") ".fish"))))
      (when extension
        (concat straight-base-dir
                "straight/repos/emacs-libvterm/etc/emacs-vterm"
                extension))))

  (defun =vterm-setup-shell ()
    "Setup the running shell for vterm."
    (let ((cmd (concat "source " (=vterm-extend-source-path))))
      (vterm-send-string cmd)
      (vterm-send-return)
      (when (equal vterm-shell "/bin/zsh")
        ;; Setup vterm_set_directory command
        (vterm-send-string
         "vterm_set_directory() {vterm_cmd update-pwd \"$PWD/\"}")
        (vterm-send-return)
        ;; Run the command before displaying the prompt
        (vterm-send-string "precmd() {vterm_set_directory}")
        (vterm-send-return))
      (vterm-clear)))

  :config
  (add-hook 'vterm-mode-hook '=vterm-setup-shell)
  (add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path) (setq default-directory path)))))




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

(custom-set-faces)
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
