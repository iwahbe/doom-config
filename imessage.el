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
    message_date DESC\nLIMIT\n    "
           (number-to-string limit)
           ";")
   buffer))

(define-derived-mode imessage-mode tabulated-list-mode
  (setq tabulated-list-format
        [("me" 3 t)
         ("date" 20 t)
         ("chat" 25 t)
         ("text" 1 nil)]))

(defun imessage-make-entries ()
  (let (res
        (next (lambda nil
                (let (r)
                  (setq r (read (current-buffer)))
                  (forward-char)
                  (if (numberp r)
                      (number-to-string r) r)))))
    (with-temp-buffer
      (imessage-chat-query
       (current-buffer)
       :limit
       (frame-height)
       :identifier imessage-conversation)
      (goto-char
       (point-min))
      (while
          (not
           (eobp))
        (setq res
              (cons
               (vector
                (funcall next) (funcall next)
                (funcall next) (funcall next))
               res))
        (forward-line 1))
      (reverse res))))


(defun imessage ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*imessage*"))
  (setq tabulated-list-entries (imessage-make-entries))
  (imessage-mode))
