;;; tikzalign.el -*- lexical-binding: t; -*-

(require 'latex)
(require 'cl)

(defun tikzalign-picture-region-p ()
  "Check if we are in a region to align text."
  (save-excursion
    (condition-case err
        (progn
          (LaTeX-find-matching-begin)
          (when (string-prefix-p
                 "\\begin{tikzpicture}"
                 (buffer-substring-no-properties (point) (line-end-position)))
            (line-move 1) (beginning-of-line)
            (let ((format-region-begin (point)))
              (LaTeX-find-matching-end)
              (line-move -1) (end-of-line)
              (cons format-region-begin (point)))))
      (error
       (message "tikzalign: %s" (cadr err))
       nil))))

(defun tikzalign--chunk-aligned-regions (start end on-chunk)
  "Chunk a region into similar parts. Calling ON-CHUNK on each part.
ON-CHUNK should accept the beginning and end of a region.
It should leave `point' at the end of the region to be parsed."
  ;; TODO get working
  (save-excursion
    (while (< start end)
      (if (string-empty-p (string-trim (thing-at-point 'line t)))
          (progn
            (funcall on-chunk start (point))
            (setq start (1+ (point))))
        (progn (line-move 1)
               (beginning-of-line))))
    (funcall on-chunk start end)))

(defun tikzalign-fill-region(&optional start end)
  "Aligns a tikz region. Aligns each continous block of nodes and path specifiers.
A tikz-region is defined to be between `\begin{tikzpicture}' and `\end{tikzpicture}'.
If start and end are provided, assumes that this defines a region. This is not confirmed."
  (interactive)
  (let ((region (if start (cons start end) (tikzalign-picture-region-p))))
    (when region
      (tikzalign--chunk-aligned-regions (car region) (cdr region) 'tikzalign-fill-section))))

(defun tikzalign--parse-line (line)
  "Parses a node style decl line into segments"
  (if (string-equal "" line)
      nil
    (let ((parse-end "")
          block-type)
      (cons (progn (cond
                    ((string-prefix-p "(" line)
                     (setq parse-end (substring line 0 (1+ (string-search ")" line)))
                           block-type 'paren))
                    ((string-prefix-p "[" line)
                     (setq parse-end (substring line 0 (1+ (string-search "]" line)))
                           block-type 'square-bracket))
                    ((string-prefix-p "{" line)
                     (setq parse-end (substring line 0 (1+ (string-search "}" line)))
                           block-type 'curly-brace))
                    ((string-prefix-p " " line)
                     (setq parse-end (substring line 0 (let ((next 0))
                                                         (while (string-prefix-p " " (substring line next))
                                                           (setq next (1+ next)))
                                                         next))
                           block-type 'text))
                    (t (setq parse-end (substring line 0 (min
                                                          (or (string-search "{" line) 1000)
                                                          (or (string-search "(" line) 1000)
                                                          (or (string-search "[" line) 1000)
                                                          (or (string-search " " line) 1000)
                                                          (length line)))
                             block-type 'text)))
                   (cons (if (string-empty-p (string-trim parse-end))
                             " " (string-trim-right parse-end))
                         block-type))
            (tikzalign--parse-line (substring line (length parse-end)))))))

(defun tikzalign--normalize-lines (lines)
  "Computes a normalized line. Then normalizes all lines in LINES."
  (let* ((normalize (lambda (normal x)
                      (let (out
                            (x x)
                            (y normal))
                        (while (or x y)
                          (cond
                           ;; We hope for a direct matching
                           ((equal (cdar x) (cdar y))
                            (push (car x) out)
                            (setq x (cdr x))
                            (setq y (cdr y)))
                           ;; what if there was emtpy text that did not match
                           ((and x (equal (cdar x) 'text)
                                 (string-empty-p (string-trim (caar x))))
                            (setq x (cdr x)))
                           ((and y (equal (cdar y) 'text)
                                 (string-empty-p (string-trim (caar y))))
                            (setq y (cdr y)))
                           ;; Note, we must clear away empty text before we do optional insertion
                           ;; if x has an optional bracket, give y an empty optional
                           ((equal (cdar x) 'square-bracket)
                            (push '("[]" . square-bracket) y))
                           ;; same idea for the other block
                           ((equal (cdar y) 'square-bracket)
                            (push '("[]" .  square-bracket) x))
                           (t
                            (user-error "Missing expression in parse found '%s' and '%s'" (cdar x) (cdar y)))))
                        (reverse (tikzalign--insert-break-text out)))))
         (standard (cl-reduce normalize lines)))
    (mapcar (lambda (x) (funcall normalize standard x)) lines)))

(defun tikzalign--insert-break-text (segment)
  "Inserts text segments in between non text segments.."
  (when segment
    (cons (car segment)
          (if (or (equal 'text (cdar segment)) (equal 'text (cdadr segment)))
              (tikzalign--insert-break-text (cdr segment))
            (cons '(" " . text) (tikzalign--insert-break-text (cdr segment)))))))

(defun tikzalign--line-max-sizes (lines)
  "Takes a parsed sequence and consistant LINES and calculates the max size of each component."
  (mapcar (lambda (x) (cons (length (car x)) (cdr x)))
          (cl-reduce (lambda (x y)
               (cl-mapcar (lambda (a b)
                            (cons (string-pad "" (max
                                    (length (car a))
                                    (length (car b))))
                                   (cdr a)))
                          x y))
             lines)))

(defun tikzalign--pad-line (segment left-pad right-pad pad-length)
  "Pads a segment between left-pad and right-pad."
  (concat left-pad (string-pad
                    (substring segment
                               (length left-pad)
                               (- (length segment) (length right-pad)))
                    (max 0 (- pad-length (+ (length left-pad) (length right-pad)))))
          right-pad))

(defun tikzalign--format-line (line guide)
  "Takes a parsed node line and formats it according to guide"
  (cl-assert (equal (length line) (length guide)))
  (mapconcat 'identity (cl-mapcar ;; Lists of unequal length will be cut off
                        (lambda (segment plan)
                          (cond
                           ((equal (cdr segment) 'square-bracket)
                            (tikzalign--pad-line (car segment) "[" "]" (car plan)))
                           ((equal (cdr segment) 'curly-brace)
                            (tikzalign--pad-line (car segment) "{" "}" (car plan)))
                           ((equal (cdr segment) 'paren)
                            (tikzalign--pad-line (car segment) "(" ")" (car plan)))
                           (t
                            (string-pad (car segment) (car plan)))))
                        line guide)
             ""))

(defun tikzalign-fill-section (start end)
  "Align the tikz graph nodes in a region. Region is specified by START and END."
  (interactive "r")
  (let* ((parsed (tikzalign--normalize-lines (mapcar 'tikzalign--parse-line
                                                     (split-string (buffer-substring-no-properties start end) "[\n]" t))))
         (max-template (tikzalign--line-max-sizes parsed)))
    (replace-region-contents start end
                             (lambda () (concat (mapconcat (lambda (x) (tikzalign--format-line x max-template))
                                                           parsed "\n")
                                                "\n")))))

(provide 'tikzalign)
