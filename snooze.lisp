(require :uiop)

;; CONFIG: Absolute path to be 100% safe
(defparameter *todo-path* (merge-pathnames "storage/shared/Documents/code/lisp-code/todo.txt"
                   (user-homedir-pathname)))

;; 1. Get the task name that was passed from the notification
(defvar *task-name* (car (uiop:command-line-arguments)))

(defun snooze-task ()
  (when *task-name*
    ;; Get current time + 5 minutes
    (multiple-value-bind (s m h)
        (decode-universal-time (+ (get-universal-time) (* 5 60)))
      (declare (ignore s))
      
      ;; Create the new line: "HH:MM Task"
      (let ((new-entry (format nil "~2,'0d:~2,'0d ~a" h m *task-name*)))
        
        ;; Write to file
        (with-open-file (stream *todo-path* :direction :output 
                                            :if-exists :append 
                                            :if-does-not-exist :create)
          (format stream "~%~a" new-entry))
        
        ;; Confirm to user
        (uiop:run-program (list "termux-notification" 
                                "--content" (format nil "Snoozed until ~2,'0d:~2,'0d" h m)))))))

(snooze-task)
