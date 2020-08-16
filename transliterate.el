;;; transliterate-mode -*- lexical-binding: t; -*-

(defgroup transliterate nil "Transliterate customizations.")

(defcustom transliterate-unit-seperator "[\\.?!][[:space:]]+"
  "The regex by which to seperate components of a translation."
  :type 'regexp :group 'transliterate
  :risky t)

(defcustom transliterate-append-on-split "."
  "`SPLIT-STRING' removes the regex split upon, this is appended onto each split."
  :type 'string :group 'transliterate)

(defcustom transliterate-start-ends ">>>"
  "The symbol that surrounds the start of a transliteration block."
  :type 'string :group 'transliterate)

(defcustom transliterate-end-ends "<<<"
  "The symbol that surrounds the end of a transliteration block."
  :type 'string :group 'transliterate)

(defcustom transliterate-original-name "original"
  "The symbol that indicates the block was taken from original text."
  :type 'string :group 'transliterate)

(defcustom transliterate-transliteration-name "transliteration"
  "The symbol that indicates the block is translated text."
  :type 'string :group 'transliterate)

(defcustom transliterate-update-on-save t
  "If summary sections should be updated on save."
  :type 'bool :group 'transliterate)

(defcustom transliterate-word-bank-name "word bank"
  "The name by which to identify a word-bank."
  :type 'string :group 'transliterate)

(defcustom transliterate-current-word-bank "default"
  "The current word bank name."
  :type 'string :group 'transliterate)

(define-minor-mode transliterate-mode
  "A minor mode to assist with translating text."
  :lighter "T"
  :group 'transliterate
  :init-value nil
  :global nil
  :after-hook (add-hook 'before-save-hook #'transliterate-update nil t)
  :keymap
  '(([C-x M-t] . transliterate-region)
    ([C-x M-u] . transliterate-add-to-word-bank) ; TODO: set this up with kbd
    ([C-x M-w] . transliterate-set-current-word-bank))
  (when (fboundp 'map!)
    (map! :map doom-leader-toggle-map
          :prefix ("t" "Transliterate")
          :desc "region" "r" 'transliterate-region
          :desc "update" "u" 'transliterate-update
          :desc "insert" "i" 'transliterate-add-to-word-bank))
  )

(defun transliterate-start-block (middle)
  (concat transliterate-start-ends " " middle " " transliterate-start-ends))

(defun transliterate-end-block (middle)
  (concat transliterate-end-ends " " middle " " transliterate-end-ends))

(defun transliterate-extract-text (&optional start end)
  (if end
      (buffer-substring-no-properties start end)
    start))

(defun transliterate-completing-read (collection &optional prompt)
  "Sensable defaults for completion-read for this mode.
`COLLECTION' provides the collection of keys to choose from.
`PROMPT' provides the prompt."
  (let ((completion-extra-properties (if (hash-table-p collection)
                                         '(:annotation-function (lambda (s) (concat " -- " (gethash s collection))))
                                       completion-extra-properties)))
    (completing-read (or prompt "Key: ") collection nil 'confirm nil nil nil t)))


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
         ((numberp name) (transliterate-completing-read (transliterate-find-name 'word-bank buffer) "Word Bank: "))
         ((stringp name) name)
         (t (assert nil nil "Invalid name for set-current-word-bank")))))

(defun transliterate-add-to-word-bank (&optional name word description buffer)
  "Add a `WORD' to the current word bank called `NAME' with value `DESCRIPTION'.
Searches for the word bank in `BUFFER'. Will ask interactivly for `WORD' `DESCRIPTION' and `NAME' if not provided."
  (interactive "P")
  (let* ((name (transliterate-set-current-word-bank name buffer))
         (dic (or (transliterate-find-word-bank name buffer) (make-hash-table :test #'equal)))
         (key (or word (transliterate-completing-read (hash-table-keys dic) "Key: " )))
         (value (or description (read-string "Value: " (gethash key dic)))))
    (puthash key value dic)
    (transliterate-update-word-bank dic name t buffer)))

(defun transliterate-remove-from-word-bank (&optional name word buffer)
  "Remove `WORD' from the current word bank (searched for in `BUFFER'), or `NAME' if provided."
  (interactive "P")
  (let* ((name (transliterate-set-current-word-bank name buffer))
         (dic (or (transliterate-find-word-bank name buffer) (make-hash-table :test #'equal)))
         (key (or word (transliterate-completing-read (hash-table-keys dic) "Key: "))))
    (remhash key dic)
    (transliterate-update-word-bank dic name nil buffer)))

(defun transliterate-normalize-whitespace (str)
  "Normalize the whitespace in a string"
  (string-trim (replace-regexp-in-string "[[:space:]]+" " " str)))

(defun transliterate-seperate-paragraph (&optional start end)
  "If start is provided and end is not, then seperate-paragraph will "
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
          (let ((txt (transliterate-join-paragraph
                      (transliterate-find-native name 'transliteration buffer))))
            (insert txt)
            (fill-region-as-paragraph (car bounds) (+ -1 (car bounds) (length txt)))
            (insert  "\n")))))))

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
          (name-regexp " \\([[:alnum:]]+\\)\\(-[0-9]+\\)?"))
      (save-excursion
        (goto-char (point-min))
        (while
            (re-search-forward (concat
                                (transliterate-start-block (transliterate-symbol-string-map type))
                                name-regexp) nil t)
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

;; TODO: set up things that accept names to force names to ba acceptable, so
;; disallow charicters such as ' ' and '-'

(provide 'transliterate)
