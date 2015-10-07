(setq key-recording-file "key_recording.txt")

(defun on-key-typed ()
  (write-event (this-command-keys-vector)))

(defun get-event-description (event)
  (concat
   (apply #'concatenate 'string (mapcar 'event-to-string event))
   "\n" (number-to-string (float-time)) "\n\n"))

(defun event-to-string (event)
  (cond
   ((stringp event) event)
   ((characterp event) (char-to-string event))
   ((integerp event) (convert-integer-event event))
   (t "error")))

(defun convert-integer-event (event)
  (let ((char (logand #xff event)))
       (cond
	((logand #x8000000 event) (concat "alt-" (char-to-string char)))
	(t (char-to-string char)))))

(defun write-event (event)
  (with-current-buffer (get-buffer key-recording-file)
    (end-of-buffer)
    (insert (get-event-description event))))

(defun save-recording ()
  (with-current-buffer (get-buffer key-recording-file)
    (save-buffer)))

(defun auto-save-key-recording ()
  (run-with-idle-timer 30 t 'save-recording))

(defun setup-key-recording ()
  (progn
    (find-file (concat "~/.emacs.d/" key-recording-file))
    (switch-to-buffer (other-buffer))
    (add-hook 'pre-command-hook 'on-key-typed)
    (add-hook 'kill-emacs-hook 'save-recording)
    (auto-save-key-recording)))

