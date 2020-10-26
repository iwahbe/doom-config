;;; ~/.doom.d/spell-fix.el -*- lexical-binding: t; -*-

(define-minor-mode spell-fix
  "Fixes common spelling mistakes as they are typed"
  :init-value nil
  :lighter spfx
  :after-hook (add-hook 'post-self-insert-hook #'spell-fix-maybe nil t)
  )

(defun spell-fix-dic (word)
  "Returns the intended word based on word."
  (let ((pure-word (replace-regexp-in-string "[.,!?]" "" word)))
  (cond
   ((equal pure-word "THe") "The")
   ((equal pure-word "THis") "This")
   ((equal pure-word "THeorem") "Theorem")
   )))


(defun spell-fix-end-of-wordp ()
  "If we are at the end of a word."
  (save-excursion
    (and (not (word-at-point))
         (progn
           (backward-char)
           (word-at-point)))
    ))

(defun spell-fix-word (&optional word)
  "Fixes the last word if it is wrong."
  (interactive)
  (let* ((fix-word (if word word (current-word)))
         (new-word (spell-fix-dic fix-word)))
    (when (and new-word (null word))
      (let ((ch-point (char-before)))
        (backward-word)
        (kill-word 1)
        (insert new-word ;;(char-to-string ch-point)
                )))
    )
  nil
  )


(defun spell-fix-maybe ()
  "When in the correct mode, calls spell-fix-word at the end of a word"
  (interactive)
  (when (and spell-fix (spell-fix-end-of-wordp))
    (spell-fix-word)
    ))


(provide 'spell-fix)
