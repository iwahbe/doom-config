;;; tikzpicture.el -*- lexical-binding: t; -*-

(require 'latex)

(defun align-tikz-region-p ()
  "Check if we are in a region to align text."
  (save-excursion
    (LaTeX-find-matching-begin)
    (when (string-prefix-p "\\begin{tikzpicture}" (buffer-substring-no-properties (point) (line-end-position)))
      (line-move 1) (beginning-of-line)
      (let ((format-region-begin (point)))
        (LaTeX-find-matching-end)
        (line-move -1) (end-of-line)
        (cons format-region-begin (point))))))


(defun parse-segment (line)
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
                    (t (setq parse-end (substring line 0 (min
                                                          (or (string-search "{" line) 1000)
                                                          (or (string-search "(" line) 1000)
                                                          (or (string-search "[" line) 1000)
                                                          (length line)))
                             block-type 'text)))
                   (cons (if (string-empty-p (string-trim parse-end))
                             " " (string-trim-right parse-end))
                         block-type))
            (parse-segment (substring line (length parse-end)))))))

(defun normalize-node-optionals (lines)
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
                            (push (cons "[]" 'square-bracket) y)
                            (push (cons " " 'text) y))
                           ;; same idea for the other block
                           ((equal (cdar y) 'square-bracket)
                            (push (cons "[]" 'square-bracket) x)
                            (push (cons " " 'text) x))
                           (t
                            (user-error "Missing expression in parse found '%s' and '%s'" (cdar x) (cdar y)))))
                        (reverse (insert-break-text out)))))
         (standard (cl-reduce normalize lines)))
    (mapcar (lambda (x) (funcall normalize standard x)) lines)))

(defun insert-break-text (segment)
  "Reverses a list, inserting text where there is none."
  (when segment
    (cons (car segment)
        (if (or (equal 'text (cdar segment)) (equal 'text (cdadr segment)))
            (insert-break-text (cdr segment))
    (cons '(" " . text) (insert-break-text (cdr segment)))))))

(defun node-line-maxes (lines)
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

(defun pad-segment-string (segment left-pad right-pad pad-length)
  "Pads a segment between left-pad and right-pad."
  (concat left-pad (string-pad
                    (substring segment
                               (length left-pad)
                               (- (length segment) (length right-pad)))
                    (max 0 (- pad-length (+ (length left-pad) (length right-pad)))))
          right-pad))

(defun format-node-line (line guide)
  "Takes a parsed node line and formats it according to guide"
  (cl-assert (equal (length line) (length guide)))
  (mapconcat 'identity (cl-mapcar ;; Lists of unequal length will be cut off
                        (lambda (segment plan)
                          (cond
                           ((equal (cdr segment) 'square-bracket)
                            (pad-segment-string (car segment) "[" "]" (car plan)))
                           ((equal (cdr segment) 'curly-brace)
                            (pad-segment-string (car segment) "{" "}" (car plan)))
                           ((equal (cdr segment) 'paren)
                            (pad-segment-string (car segment) "(" ")" (car plan)))
                           (t
                            (string-pad (car segment) (car plan)))))
                        line guide)
             ""))

(defun fill-tikz-region (start end)
  "Align the tikz graph nodes in a region. Region is specified by START and END."
  (interactive "r")
  (let* ((parsed (normalize-node-optionals (mapcar 'parse-segment
                         (split-string (buffer-substring-no-properties start end) "[\n]" t))))
         (max-template (node-line-maxes parsed)))
    (replace-region-contents start end
                             (lambda () (concat (mapconcat (lambda (x) (format-node-line x max-template))
                                                      parsed "\n")
                                           "\n")))))
