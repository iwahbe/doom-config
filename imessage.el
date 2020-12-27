;;; imessage.backup.el -*- lexical-binding: t; -*-

(defun imessage nil
  (interactive)
  (get-buffer-create "*imessage*")
  (imessage-mode)
  (setq tabulated-list-entries
        (imessage-make-entries))
  (tabulated-list-print))

(defvar imessage-executable "/usr/bin/sqlite3"
  "The executable used to query chat.db.")

(defvar imessage-database "/Users/ianwahbe/Library/Messages/chat.db"
  "The path of the imessage database.")

(defvar-local imessage-conversation nil
  "Current imessage conversation, nil is the conversation selector.")

(defun imessage-query
    (query buffer)
  "Querys the imessage database with QUERY, dumping it's output into BUFFER"
  (call-process imessage-executable nil buffer nil "--csv" imessage-database query))

(cl-defun imessage-chat-query (buffer &key (limit 10) (identifier nil))
  (imessage-query
   (concat "SELECT
        message.is_from_me,
        datetime (message.date / 1000000000 + strftime (\"%s\", \"2001-01-01\"), \"unixepoch\", \"localtime\") AS message_date,
        chat.chat_identifier,
        message.text
    FROM
        chat
    JOIN chat_message_join ON chat. \"ROWID\" = chat_message_join.chat_id
    JOIN message ON chat_message_join.message_id = message. \"ROWID\""
           (if identifier
               (progn
                 (concat "\nWHERE chat.chat_identifier = '" identifier "'")))
           "\nORDER BY
    message_date DESC
    LIMIT "
           (number-to-string limit)
           ";")
   buffer))

(cl-defun imessage-chat-identifiers-query (buffer &key (limit 10))
  (imessage-query
   (concat "SELECT DISTINCT
        chat.chat_identifier
    FROM
        chat
    JOIN chat_message_join ON chat. \"ROWID\" = chat_message_join.chat_id
    JOIN message ON chat_message_join.message_id = message. \"ROWID\"
    ORDER BY
        message_date ASC
    LIMIT "
           (number-to-string limit)
           ";")
   buffer))

(defun imessage-chat-identifiers ()
  "Returns a list of valid chat identifiers, and nil"
  (with-temp-buffer
    (imessage-chat-identifiers-query (current-buffer) :limit 1000)
    (mapcar (lambda (x) (elt x 0)) (imessage-read-csv (current-buffer) 1))))

(defun imessage-select-conversation (&optional conversation)
  "Selects a conversation to focus on."
  (interactive)
  (when conversation (cl-assert (stringp conversation)))
  (setq imessage-conversation (or conversation
                                  (completing-read "Chat: " (cons nil (imessage-chat-identifiers)) nil t)))
  (imessage-refresh))

(defun imessage-refresh ()
  "Refreshes the contents of an imessage-mode buffer."
  (interactive)
  (setq tabulated-list-entries (imessage-make-entries))
  (tabulated-list-revert))

(define-derived-mode imessage-mode tabulated-list-mode
  (setq tabulated-list-format
        [("me" 3 t)
         ("date" 20 t)
         ("chat" 25 t)
         ("text" 1 nil)])
  (tabulated-list-init-header))

(defun imessage-read-csv (buffer ncolumns)
  "Reads a CSV with NCOLUMNS in BUFFER."
  (cl-assert (> ncolumns 0))
  (let (res)
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (not (eobp))
        (setq res (cons
                   (eval `(vector ,@(mapcar 'funcall
                                            (make-list ncolumns 'imessage-read-csv-field))))
                   res)))
      (reverse res))))

(defun imessage-read-csv-field ()
  "Parses a csv field starting at point in the current buffer.
Fields are seperated by commas or new lines. If A field contains either, it must be quoted.
Quotes are escaped by quotes. We handle this with a finite state machine.

NO-ESCAPE != \" -> NO-ESCAPE | == \" -> maybe-done
MAYBE-DONE == \" -> NO-ESCAPE | == \" -> DONE
"
  (string-replace "\n" "\\n" (if (= (char-after) ?\")
                                 (let ((start (1+ (point)))
                                       maybe-escaped done)
                                   (forward-char)
                                   (while (not done)
                                     (cond
                                      ((and maybe-escaped (= (char-after) ?\"))
                                       (setq maybe-escaped nil))
                                      ;; We know that the next char is not ", so we esacape.
                                      (maybe-escaped (setq done t))
                                      ;; We know that maybe-escaped is false, and we hit a ", so we set it.
                                      ((= (char-after) ?\")
                                       (setq maybe-escaped t)))
                                     (forward-char))
                                   (buffer-substring-no-properties start (- (point) 2)))
                               (let ((start (point)))
                                 (re-search-forward "[,\n]")
                                 (buffer-substring-no-properties start (1- (point)))))))

(defun imessage-make-entries ()
  "Generates entries suitable for `tabulated-list-entries'"
  (let ((query-ident imessage-conversation))
    (with-temp-buffer
      (imessage-chat-query (current-buffer)
                           :limit (* 2 (frame-height))
                           :identifier query-ident)
      (mapcar (lambda (e) (list nil e)) (imessage-read-csv (current-buffer) 4)))))


(defun imessage ()
  "Sets up imessage."
  (interactive)
  (switch-to-buffer (get-buffer-create "*imessage*" t) t)
  (setq tabulated-list-entries (imessage-make-entries))
  (imessage-mode)
  (tabulated-list-print))
