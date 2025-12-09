(require :uiop)

;; CONFIG: Absolute paths to ensure stability
(defparameter *todo-path* (merge-pathnames "storage/shared/Documents/code/lisp-code/todo.txt"
                   (user-homedir-pathname)))

(defparameter *snooze-script* (merge-pathnames "storage/shared/Documents/code/lisp-code/snooze.lisp"
                   (user-homedir-pathname)))

(defun current-time-string ()
  (multiple-value-bind (s m h) (get-decoded-time)
    (declare (ignore s))
    (format nil "~2,'0d:~2,'0d" h m)))

(defun send-alert (message)
  (format t "[ALARM] ~a~%" message)
  
  ;; The Command: sbcl --script /path/to/snooze.lisp "Task Name"
  (let ((snooze-cmd (format nil "sbcl --script ~a \"~a\"" *snooze-script* message)))

    (uiop:run-program (list "termux-notification"
                            "--title" "Task Reminder"
                            "--content" message
                            "--id" "lisp-scheduler"
                            "--priority" "high"
                            "--sound"
                            ;; THE BUTTON
                            "--button1" "Snooze 5m"
                            "--button1-action" snooze-cmd)
                      :ignore-error-status t)))

(defun check-tasks ()
  (let ((now (current-time-string)))
    ;; Heartbeat (prints a dot so you know it's running)
    (format t ".") 
    (force-output)
    
    (if (probe-file *todo-path*)
        (let ((lines (uiop:read-file-lines *todo-path*)))
          (dolist (line lines)
            ;; Parse lines like "00:00 Task"
            ;; Check length > 6 and ensure the 3rd char (index 2) is a colon
            (when (and (> (length line) 6) 
                       (char= (char line 2) #\:))
              (let ((task-time (subseq line 0 5))
                    (task-msg  (subseq line 6)))
                ;; If time matches, fire alert
                (when (string= task-time now)
                  (send-alert task-msg))))))
        (format t "[Error] File not found: ~a~%" *todo-path*))))

(defun main ()
  (format t "Scheduler Started. (Ctrl+C to stop)~%")
  (loop
    (check-tasks)
    ;; Wait 60 seconds so we don't spam notifications
    (sleep 60)))

(main)
