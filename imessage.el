;;; imessage.backup.el -*- lexical-binding: t; -*-

(defvar imessage-executable "/usr/bin/sqlite3"
  "The executable used to query chat.db.")

(defvar imessage-database "/Users/ianwahbe/Library/Messages/chat.db"
  "The path of the imessage database.")

(defvar imessage--identifier-query-part "CASE
            WHEN chat.display_name IS NOT \"\" THEN chat.display_name
            ELSE chat.chat_identifier
            END AS name" "The part of the SQLite query that forms the name identifier.")

(defvar-local imessage-conversation nil
  "Current imessage conversation of the form (display name . conversation id), nil is the conversation selector.")

(defun imessage-query (query buffer)
  "Querys the imessage database with QUERY, dumping it's output into BUFFER"
  (when (not (= 0 (call-process imessage-executable nil (list buffer t) nil "--csv" imessage-database query)))
    (user-error "imessage: %s" (buffer-substring-no-properties (point-min) (min (point-max) 200)))))

(cl-defun imessage-chat-query (buffer &key (limit 10) (identifier nil))
  "Fetches chat data from the server. The format is as follows: [is_from_me, datetime sent, chat identifier, text]."
  (imessage-query
   (concat "SELECT
            message.is_from_me,
            datetime (message.date / 1000000000 + strftime (\"%s\", \"2001-01-01\"), \"unixepoch\", \"localtime\") AS message_date,"
           imessage--identifier-query-part ;; Generates `name'
           ",\n
            message.text
            FROM
            chat
            JOIN chat_message_join ON chat. \"ROWID\" = chat_message_join.chat_id
            JOIN message ON chat_message_join.message_id = message. \"ROWID\""
           (if identifier
               (progn
                 (concat "\nWHERE name = '" identifier "'")))
           "\nORDER BY
            message_date DESC
            LIMIT "
           (number-to-string limit)
           ";")
   buffer))

(cl-defun imessage-chat-identifiers-query (buffer &key (limit 10))
  "Fetches valid identifiers for conversations."
  (imessage-query
   (concat "SELECT DISTINCT
        " imessage--identifier-query-part ",
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
    (mapcar (lambda (x) (cons (elt x 0) (elt x 1)))
            (imessage-read-csv (current-buffer) 2))))

(defun imessage-select-conversation (&optional conversation)
  "Selects a conversation to focus on."
  (interactive)
  (when conversation (cl-assert (stringp conversation)))
  (let ((idents (imessage-chat-identifiers)))
    (setq imessage-conversation (or conversation
                                    (let ((res (completing-read "Chat: " (cons nil idents) nil t)))
                                      (seq-find (lambda (el) (equal (car el) res)) idents nil)))))
  (imessage-refresh)
  (tabulated-list-revert)
  imessage-conversation)

(defun imessage-refresh ()
  "Refreshes the contents of an imessage-mode buffer."
  (interactive)
  (setq tabulated-list-entries (imessage-make-entries)))

(define-derived-mode imessage-mode tabulated-list-mode
  (setq tabulated-list-format
        [("me" 3 t)
         ("date" 20 t)
         ("chat" 25 t)
         ("text" 1 nil)])
  (add-hook 'tabulated-list-revert-hook 'imessage-refresh nil t)
  (define-key imessage-mode-map (kbd "s") 'imessage-send-message)
  (define-key imessage-mode-map (kbd "c") 'imessage-select-conversation)
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
  (string-replace
   "\n" "\\n"
   (if (= (char-after) ?\")
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
  (let ((query-ident (car imessage-conversation)))
    (with-temp-buffer
      (imessage-chat-query (current-buffer)
                           :limit (* 10 (frame-height))
                           :identifier query-ident)
      (mapcar (lambda (e) (list nil e)) (imessage-read-csv (current-buffer) 4)))))


(defun imessage ()
  "Sets up an interactive imessage view."
  (interactive)
  (switch-to-buffer (get-buffer-create "*imessage*") t)
  (setq tabulated-list-entries (imessage-make-entries))
  (imessage-mode)
  (tabulated-list-print))

(defun imessage--applescript (contents &option error)
  "Send a applescript event to Messages."
  (unless (= 0 (call-process "osascript" nil nil nil
                             "-e"
                             (concat "tell application \"Messages\" to " contents)))
    (error (concat "imessage: " (or error "somthing failed")))))

(defun imessage-send-message (&optional recipient message)
  "Send MESSAGE to RECIPIENT."
  (interactive)
  (let ((to (or recipient imessage-conversation (imessage-select-conversation))))
    (imessage--applescript (concat
                            "send \"" (or message
                                          (read-from-minibuffer (concat "Message to " (car to) ": ")))
                            "\" to text chat \"" (cdr to) "\""))))
