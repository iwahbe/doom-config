;;; ~/.doom.d/transliterate.el -*- lexical-binding: t; -*-

(defvar transliterate-unit-seperator "\\.[[:space:]]+"
  "The regex by which to seperate components of a translation.")
(defvar transliterate-append-on-split "."
  "`SPLIT-STRING' removes the regex split upon, this is appended onto each split.")

(defvar transliterate-start-ends ">>>"
  "The symbol that surrounds the start of a transliteration block.")

(defvar transliterate-end-ends "<<<"
  "The symbol that surrounds the end of a transliteration block.")

(defvar transliterate-original-name "original"
  "The symbol that indicates the block was taken from original text.")

(defvar transliterate-transliteration-name "transliteration"
  "The symbol that indicates the block is translated text.")

(defvar transliterate-update-on-save t
  "If summary sections should be updated on save.")

(defvar transliterate-word-bank-name "word bank"
  "The name by which to identify a word-bank.")

(defvar transliterate-current-word-bank "default"
  "The current word bank name.")

(defvar transliterate-word-bank-history nil
  "The word bank history")

(define-minor-mode transliterate-mode
  "A minor mode to assist with translating text."
  :lighter "T"
  :init-value nil
  :global nil
  :after-hook (add-hook 'before-save-hook #'transliterate-update nil t)
  :keymap
  '(([C-x M-t] . transliterate-region)
    ([C-x M-u] . transliterate-update))
  )

(defun transliterate-start-block (middle)
  (concat transliterate-start-ends " " middle " " transliterate-start-ends))

(defun transliterate-end-block (middle)
  (concat transliterate-end-ends " " middle " " transliterate-end-ends))

(defun transliterate-extract-text (&optional start end)
  (if end
      (buffer-substring-no-properties start end)
    start))


(defun transliterate-show-word-bank (&optional name key dictionary hist)
  ;; TODO: set up for interactive use
  "User interface for a dictionary.
`DICTIONARY' is a hash-table, and `HIST' is a history list for the dictionary."
  (interactive "P")
  (let ((key (completing-read "Word: " (hash-table-keys dictionary) nil 'confirm nil hist nil t)))
    ;; TODO: should be it's own buffer
    (let ((value (gethash key dictionary)))
      (message "%s: %s" key value)
      (list value (cons key hist)))))

(defun transliterate-update-word-bank (dictionary name &optional force buffer)
  "Writes `DICTIONARY' in a human readable format to the word bank.
If `FORCE' is non-nil, then create a word-bank at the end of the document if none is found.
Search for and operate in `BUFFER' if non-nil."
  (with-current-buffer (if buffer buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (let ((bounds (transliterate-find-between-tags
                     (concat (transliterate-start-block transliterate-word-bank-name) " " name "\n")
                     (transliterate-end-block transliterate-word-bank-name) t))
            (do-insert (lambda (bounds)
                         (delete-region (car bounds) (cdr bounds))
                         (goto-char (car bounds))
                         (insert
                                  (mapconcat (lambda (key) (format "%S" (list key (gethash key dictionary))))
                                          (hash-table-keys dictionary) "\n") "\n"))))
        (if bounds
            (funcall do-insert bounds)
          (when force
            (goto-char (point-max))
            (insert "\n"
                    (transliterate-start-block transliterate-word-bank-name) " " name
                    "\n")
            (funcall do-insert (cons (point-max) (point-max)))
            (insert "\n" (transliterate-end-block transliterate-word-bank-name))))))))

(defun transliterate-find-word-bank (name &optional buffer)
  "Searches for a word-bank hash-map called `NAME' in `BUFFER'."
  (let ((contents (car (read-from-string
                        (let ((input
                               (mapconcat #'identity (transliterate-find-native name 'word-bank buffer) " ")))
                          (if (string-empty-p (string-trim input)) "nil" (concat "(" input")"))))))
        (table (make-hash-table :test #'equal)))
    (while contents
      (let ((item (pop contents)))
      (puthash (car item) (cadr item) table)))
    table))

(defun transliterate-set-current-word-bank (&optional name buffer)
  "Sets the current word bank to `NAME' if it exists, otherwise it asks.
The list of names is gathered by scanning `BUFFER'."
  (interactive)
  (setq transliterate-current-word-bank
        (cond
         ((not name) transliterate-current-word-bank)
         ((numberp name) (completing-read "Word Bank: " (transliterate-find-name 'word-bank buffer)
                                          nil 'confirm nil nil nil t))
         ((stringp name) name)
         (t (assert nil nil "Invalid name for set-current-word-bank")))))

(defun transliterate-add-to-word-bank (&optional name word description buffer)
  "Add a `WORD' to the current word bank called `NAME' with value `DESCRIPTION'.
Searches for the word bank in `BUFFER'. Will ask interactivly for `WORD' `DESCRIPTION' and `NAME' if not provided."
  (interactive "P")
  (let* ((name (transliterate-set-current-word-bank name buffer))
         (dic (or (transliterate-find-word-bank name buffer) (make-hash-table :test #'equal)))
         (key (or word (completing-read "Key: " (hash-table-keys dic) nil 'confirm)))
         (value (or description (read-string "Value: " (gethash key dic)))))
    (puthash key value dic)
    (transliterate-update-word-bank dic name t buffer)))

(defun transliterate-remove-from-word-bank (&optional name word buffer)
  "Remove `WORD' from the current word bank (searched for in `BUFFER'), or `NAME' if provided."
  (interactive "P")
  (let* ((name (transliterate-set-current-word-bank name buffer))
         (dic (or (transliterate-find-word-bank name buffer) (make-hash-table :test #'equal)))
         (key (or word (completing-read "Key: " (hash-table-keys dic) nil t nil))))
    (remhash key dic)
    (transliterate-update-word-bank dic name nil buffer)))

(defun transliterate-normalize-whitespace (str)
  "Normalize the whitespace in a string"
  (string-trim (replace-regexp-in-string "[[:space:]]+" " " str)))

(defun transliterate-seperate-paragraph (&optional start end)
  "If start is provided and end is not, then seperate-paragraph will "
  (interactive "r")
  (let ((region (transliterate-normalize-whitespace (transliterate-extract-text start end))))
    (mapcar (lambda (a) (concat a "."))
            (split-string region transliterate-unit-seperator t "[[:space:]]+"))))

(defun org-dblock-write:transliterate (params)
  "Refreshes a dynamic org block to hold the current translation in it's entirety."
  (transliterate-join-paragraph params))

(defun transliterate-join-paragraph (sentences)
  "Joins a list: `SENTENCES' back into a paragraph."
  (transliterate-normalize-whitespace (mapconcat #'identity sentences " ")))

;; (org-dynamic-block-define "transliterate" #'org-dblock-write:transliterate)

(defun transliterate-org-sentence-block (sentence name &optional number secondary-param-list)
  "Formats the sentence as a org-block.
`SENTENCE' is the sentence text.
`NUMBER' is the sentence number.
`NAME' is the name of the transliteration region."
  (let ((label (transliterate-block-label name number)))
    (concat
     (transliterate-org-block (concat label "-" transliterate-original-name) sentence)
     "\n"
     (transliterate-org-block (concat label "-" transliterate-transliteration-name)
                              nil secondary-param-list))))

(defun transliterate-native-sentence-block (sentence name &optional number discard)
  (concat
   (transliterate-native-block sentence name number nil)
   "\n"
   (transliterate-native-block nil name number t)))

(defun transliterate-native-block (sentence name &optional number translationp)
  (let ((type (if translationp transliterate-transliteration-name transliterate-original-name)))
    (concat (transliterate-start-block type)
            " " (transliterate-block-label name number) "\n"
            sentence "\n"
            (transliterate-end-block type) "\n"
            )))

(defun transliterate-find-between-tags (start end &optional rawp)
  "Finds the text between start and end, leaving point at end."
  (save-match-data
    (let* ((begin (re-search-forward start nil t))
           (end (when begin (re-search-forward end nil t))))
      (when end
        (if rawp
            (cons begin (match-beginning 0))
          (buffer-substring-no-properties begin (match-beginning 0)))))))

(defun transliterate-native-update-summary (name &optional buffer)
  (with-current-buffer (if buffer buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (let ((bounds (transliterate-find-between-tags
                     (concat (transliterate-start-block transliterate-transliteration-name)
                             " " name "\n")
                     (transliterate-end-block transliterate-transliteration-name) t)))
        (when bounds
          (delete-region (car bounds) (cdr bounds))
          (goto-char (car bounds))
          (insert (transliterate-join-paragraph
                   (transliterate-find-native name 'transliteration buffer)) "\n"))))))

(defun transliterate-symbol-string-map (symbol)
  "Maps between types (symbols) and strings."
  (cond ((eq symbol 'transliteration) transliterate-transliteration-name)
        ((eq symbol 'original) transliterate-original-name)
        ((eq symbol 'word-bank) transliterate-word-bank-name)))

(defun transliterate-find-native (name &optional type buffer)
  "Find all blocks with name `NAME' and `TYPE' in `BUFFER'."
  (with-current-buffer (if buffer buffer (current-buffer))
    (let ((search-start (concat (transliterate-start-block (transliterate-symbol-string-map type))
                                " " name "\\(-\[0-9\]+\\)?"))
          (search-end (transliterate-end-block (transliterate-symbol-string-map type)))
          (text-body nil)
          (sentences '()))
      (save-excursion
        (goto-char (point-min))
        (while (setq text-body (transliterate-find-between-tags search-start search-end))
          (push text-body sentences)
          ))
      (reverse sentences))))

(defun transliterate-block-label (name &optional number)
  (concat name (when number (concat "-" (number-to-string number)))))

(defun transliterate-org-block (name &optional content param-list)
  "Creates a block with name `NAME' and content `CONTENT'.
If `PARAM-LIST' is non-nil, then this is a dynamic block"
  (concat
   "#+NAME " name "\n"
   "#+BEGIN" (when param-list ": ") (mapconcat #'identity param-list " ") "\n"
   content "\n"
   "#+END" (when param-list ":") "\n"))

(defun transliterate-get-type (&optional type)
  "Determines if running for org or native.
Valid types are `\'native' and `\'org'."
  (if type (progn
             (assert (or (eq 'org type) (eq 'native type))) type)
    (if (eq major-mode 'org-mode)
        'org
      'native)))

(defun transliterate-region (start &optional end name type)
  "Writes a transliteration template of the region described between `START' and `END'
or just the string `START' if end is `nil'.
Valid types are `nil' which means dwim, `\'org' which forces org style or `\'native' which forces native style."
  (interactive "r")
  (let* ((original-text (transliterate-extract-text start end))
         (sentences (transliterate-seperate-paragraph original-text))
         (name (or name (read-string "Transliteration name: ")))
         (normalized-type (transliterate-get-type type))
         (sentence-block (if (eq 'native normalized-type)
                             #'transliterate-native-sentence-block
                           #'transliterate-org-sentence-block))
         (summary-block (funcall sentence-block original-text name nil
                                 (list ":blocks" (concat "\"" name "-[[:alph:]]+-transliteration\""))))
         (sentence-blocks (cl-mapcar (lambda (sentence number)
                                       (funcall sentence-block sentence name number))
                                     sentences (number-sequence 1 (length sentences)))))
    (let ((result (concat summary-block "\n\n" (mapconcat #'identity sentence-blocks "\n\n")
                          )))
      ;; Test if the function was called on a string or a region
      (if end (progn
                (delete-region start end) (insert result)
                (goto-char start))
        result))))

(defun transliterate-find-name (type &optional buffer)
  (with-current-buffer (if buffer buffer (current-buffer))
    (let ((out-list '())
          (found-text nil)
          (name-regexp " \\([[:alnum:]]+\\)\\(-[0-9]+\\)?"))
      (save-excursion
        (goto-char (point-min))
        (while
            (setq found-text (re-search-forward
                              (concat (transliterate-start-block (transliterate-symbol-string-map type))
                                      name-regexp) nil t))
          (unless (member (match-string-no-properties 1) out-list)
            (push (match-string-no-properties 1) out-list)))
        )
      out-list)))

(defun transliterate-find-names (&optional buffer)
  "Find names of translation segments in `BUFFER', returning a list of such names."
  (let ((original (transliterate-find-name 'original buffer))
        (transliteration (transliterate-find-name 'transliteration buffer)))
    (cl-intersection original transliteration :test #'equal)))

(defun transliterate-update ()
  "Updates all block summaries."
  (interactive)
  (when (and transliterate-mode transliterate-update-on-save)
    (let ((names (transliterate-find-names)))
      (while names
        (transliterate-native-update-summary (pop names))))))
