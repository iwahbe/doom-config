;;; confirm-init.el -*- lexical-binding: t; -*-
;;;
;;; Assuming that the `doom' example `init.el' file is used, we check to see if
;;; all modules are mentioned, either turned on or in the comments.

(defvar =do-check-doom!-init t
  "Whiether to check the doom init on startup")

(defun =check-doom!-init (&optional force)
  (when (or =do-check-doom!-init force)
    (let* ((init-modules (=modules-in-doom!-init))
           (fs-modules (=all-doom!-fs-modules))
           (deficit (cl-set-difference fs-modules init-modules :test 'equal)))
      (=show-doom!-results deficit))))


(defun =show-doom!-results (deficit)
  "Display recomendations to keep an up to date `init.el'.
Recomendataions are drawn from `init.example.el'"
  (if deficit
      (let ((message "The following modules were not mentioned:\n"))
        (with-temp-buffer
          (insert (=find-function-in-file (concat doom-emacs-dir "init.example.el") 'doom!))
          (mapc (lambda (package)
                  (goto-char (point-min))
                  (setq message
                        (concat message
                                (if (search-forward-regexp
                                     (concat "^[\n\t ;]*"(symbol-name (cdr package))) nil t)
                                    (format "%s %s\n%s\n" (car package) (cdr package)
                                            (buffer-substring-no-properties
                                             (line-beginning-position) (line-end-position)))
                                  (format "%s %s\n" (car package) (cdr package))))))
                deficit))
        (+popup-buffer (with-current-buffer (get-buffer-create "doom! missing")
                         (erase-buffer)
                         (insert message)
                         (current-buffer))))
    (let ((shown (get-buffer-window "doom! missing")))
      (when shown
        (delete-window shown)))))


(defun =modules-in-doom!-init ()
  "Find all modules in the `init.el' file."
  (let* ((init (concat doom-private-dir "/init.el"))
         (doom-call (=find-function-in-file init 'doom!))
         (uncommented (read (=remove-leading-comment-chars doom-call))))
    (=parse-doom!-asoc-list (cdr uncommented))))


(defun =all-doom!-fs-modules ()
  "Returns a list of the form '((moduel . name)) for all modules described in the file system."
  (let* ((no-self (lambda (dir) (let ((base (file-name-nondirectory dir)))
                                  (not (or (string= base ".") (string= base ".."))))))
         (search-path (lambda (dir)
                        (when
                            (and (file-exists-p dir) (funcall no-self dir))
                          (directory-files dir t)))))
    (mapcar (lambda (path) (cons
                            (intern (concat ":" (file-name-nondirectory
                                                 (string-remove-suffix "/" (file-name-directory path)))))
                            (intern (file-name-nondirectory path))))
            (seq-filter no-self (flatten-list
                                 (mapcar search-path
                                         (flatten-list (mapcar search-path doom-modules-dirs))))))))


(defun =parse-doom!-asoc-list (list &optional header)
  "Parse a list (:tag obj1 ojb2 :tag3 obj3) into ((:tag . obj1) (:tag . obj2) (:tag3 obj3)).
`header' is the initial header and `list' is the list."
  (when list
    (if (string-prefix-p ":" (symbol-name (=parse-doom!-entry (car list))))
        (=parse-doom!-asoc-list (cdr list) (car list))
      (cons (cons header (=parse-doom!-entry (car list)))
            (=parse-doom!-asoc-list (cdr list) header)))))


(defun =parse-doom!-entry (entry)
  (cond
   ((and (listp entry) (equal :if (car entry))) (caddr entry))
   ((listp entry) (car entry))
   (t entry)))


(defun =find-function-in-file (file function)
  "Returns the string matching `function's first invocation in `file'."
  (with-temp-buffer
    (insert-file-contents file)
    (let (emacs-lisp-mode-hook
          func-call)
      (emacs-lisp-mode)
      (while (not func-call)
        (parse-partial-sexp (point) (point-max) nil t)
        (let* ((start (point))
               (end (scan-sexps (point) 1))
               (guess (buffer-substring-no-properties start end)))
          (if (equal function (car (read guess)))
              (setq func-call guess)
            (goto-char end))))
      func-call)))


(defun =remove-leading-comment-chars (text)
  "Remove leading `;' from every line of `text'."
  (mapconcat (lambda (x) (replace-regexp-in-string "^[ \t\n\r]*;*" "" x))
             (split-string text "\n") "\n"))
