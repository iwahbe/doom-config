;;; evil-structures.pl -*- lexical-binding: t; -*-
;;;
;;; Provides the `evil-structured' state.
;;;
;;; It uses `tree-sitter' to allow editing on ast. Any language can use basic
;;; functionality, but it is helpful to work on a language specific subset of
;;; `node's.
;;;
;;; It can be further extended to support insertions of new nodes. This is
;;; language specific.

(require 'cl-lib)

(defvar-local evil-structured--current-node nil
  "The current node to navigate from")

(defvar evil-structured-ignore-node-list '("{" "}" "for" "in" "struct" "let" ";" "if" "loop" "|" "|" "fn" "mut" "=" "::" "<" ">" "(" ")" ",")
  "Node types to skip over")

(defun evil-structured-get-node (&optional new)
  "Returns the current node. If `new', calculates a new current
node from point in buffer"
  (unless (tree-sitter-mode +1)
    (error "evil-structured depends on tree-sitter"))
  (or (and (not new) evil-structured--current-node)
      (setq evil-structured--current-node (tree-sitter-node-at-point))))

(defun evil-structured-goto (&optional node)
  "Set point to `node'. Defaults to `evil-structured-get-node'."
  (let ((node (or node (evil-structured-get-node))))
    (push-mark (tsc-node-end-position node) t nil)
    (goto-char (tsc-node-start-position node)))
  (let ((activate-mark-hook (remove 'evil-visual-activate-hook activate-mark-hook)))
    (activate-mark t)))


(cl-defmacro evil-structured-defun-motion (name doc &rest body &key (after 'evil-structured-next-sib) &allow-other-keys)
  "Creates a function called `name' that moves the cursor to the
  node returned by `body', setting that as
  `evil-structured--current-node'. If the return value is `nil',
  then no action is taken. `:after' specifies a function to all
  on finding a trivial node (as defined by
  `evil-structured-ignore-node-list') It defaults to `evil-structured-next-sib'"
  (cl-remf body :after)
  `(evil-define-motion ,name (count)
     ,doc
     (dotimes (_ (or count 1))
       (let ((res-node (eval (let () ,@body) t)))
         (when res-node
           (evil-structured-goto (setq evil-structured--current-node res-node))
           (when (member (tsc-node-type res-node) evil-structured-ignore-node-list)
             (funcall ,after)))))))



(evil-structured-defun-motion evil-structured-parrent
                      "Navigate to the parrent"
                      (tsc-get-parent (evil-structured-get-node))
                      )

(evil-structured-defun-motion evil-structured-next-sib
                      "Navigate to the next sibling" :after 'evil-structured-next-sib
                      (let ((next (tsc-get-next-sibling (evil-structured-get-node))))
                        (or next
                          (and (evil-structured-parrent) (evil-structured-next-sib) (evil-structured-child)))))

(evil-structured-defun-motion evil-structured-prev-sib
                      "Navigate to the previous sibling" :after 'evil-structured-prev-sib
                      (let ((prev (tsc-get-prev-sibling (evil-structured-get-node))))
                        (or prev
                            (and (evil-structured-parrent) (evil-structured-prev-sib) (evil-structured-child)))))

(evil-structured-defun-motion evil-structured-child
                      "Navigate to the first child of a node" :after 'evil-structured-next-sib
                      (tsc-get-nth-child (evil-structured-get-node) 0))

(cl-defmacro evil-structured-setup-teardown (&rest key-bind-pairs)
  "Defines functions to setup and teardown local bindings"
  (let* ((base '(evil-define-minor-mode-key '(visual normal) 'evil-structured-mode-map))
         (setup (mapcar (lambda (pair) (append base pair)) key-bind-pairs))
         (teardown (mapcar (lambda (pair) (append base (list (car pair) nil))) key-bind-pairs)))
  `(progn
    (defun evil-structured-binding-setup () "Setup local keybindings for evil-structured-mode" ,@setup)
    (defun evil-structured-binding-teardown () "Teardown local keybindings for evil-structured-mode" ,@teardown)
    )))

(evil-structured-setup-teardown
  ("k" 'evil-structured-parrent)
  ("j" 'evil-structured-child)
  ("l" 'evil-structured-next-sib)
  ("h" 'evil-structured-prev-sib))

(defun evil-structured-activate ()
  "Sets up evil-structured mode for use"
  (require 'tree-sitter)
  (require 'tree-sitter-langs)
  (unless tree-sitter-mode
    (tree-sitter-mode))
  (setq evil-structured--current-node nil)
  (evil-save-transient-mark-mode)
  (transient-mark-mode)
  (evil-structured-binding-setup)
  (message "Activated structured mode") ;TODO remove debug `message'
  )

(defun evil-structured-deactivate ()
  "Restores evil to normal"
  (deactivate-mark)
  (evil-restore-transient-mark-mode)
  (evil-structured-binding-teardown)
  (message "Deactivated structured mode") ;TODO remove debug `message'
  )


(define-minor-mode evil-structured-mode
  "Structured editing for normal mode"
  :lighter "Struct"
  :keymap (make-sparse-keymap)
  (if evil-structured-mode
      (evil-structured-activate)
    (evil-structured-deactivate))
  )
