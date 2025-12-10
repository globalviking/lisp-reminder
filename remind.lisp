;;;; remind.lisp
;;;; SBCL + Termux scheduler (no ASDF/UIOP)
;;;; Checks effectively once per minute and deduplicates alerts.

(in-package :cl-user)

;; ------------------------------------------------------------
;; CONFIG: Exact Termux shared-storage paths
;; ------------------------------------------------------------

(defparameter *todo-path*
  #p"~/storage/shared/Documents/code/lisp-code/todo.txt")

(defparameter *snooze-script*
  #p"~/storage/shared/Documents/code/lisp-code/snooze.lisp")

;; Maps Lisp's day number (0=Mon, 6=Sun) to a 3-letter string
(defparameter *day-map*
  '("MON" "TUE" "WED" "THU" "FRI" "SAT" "SUN"))

;; ------------------------------------------------------------
;; Dedup guard
;; Stores a key like "MON 09:30"
;; ------------------------------------------------------------

(defparameter *last-alert-key* nil)

;; ------------------------------------------------------------
;; Utilities
;; ------------------------------------------------------------

(defun read-file-lines (path)
  (with-open-file (in path :direction :input)
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defun trim-line (s)
  (string-trim '(#\Space #\Tab #\Return #\Newline) s))

(defun run-termux-notification (args)
  ;; If PATH is flaky, replace "termux-notification" with:
  ;; "/data/data/com.termux/files/usr/bin/termux-notification"
  (sb-ext:run-program "termux-notification" args
                      :search t
                      :wait t
                      :output *standard-output*
                      :error *error-output*))

;; ------------------------------------------------------------
;; TIME
;; ------------------------------------------------------------

(defun current-time-details ()
  "Returns three values: DOW string (MON), Hour (19), Minute (30)."
  (multiple-value-bind (s m h day-of-month month year day-of-week)
      (get-decoded-time)
    (declare (ignore s day-of-month month year))
    (values (nth day-of-week *day-map*) h m)))

(defun current-time-string (h m)
  "Formats hour and minute into a string like 'HH:MM'."
  (format nil "~2,'0d:~2,'0d" h m))

(defun current-minute-key (dow h m)
  (format nil "~a ~a" dow (current-time-string h m)))

;; ------------------------------------------------------------
;; ALERT
;; ------------------------------------------------------------

(defun send-alert (message)
  (let ((msg (trim-line message)))
    (when (> (length msg) 0)
      (format t "~%[ALARM] ~a~%" msg)
      (force-output)
      (let ((snooze-cmd (format nil "sbcl --script ~a \"~a\""
                                (namestring *snooze-script*)
                                msg)))
        (run-termux-notification
         (list "--title" "Task Reminder"
               "--content" msg
               "--id" "lisp-scheduler"
               "--priority" "high"
               "--sound"
               "--icon" "done"
               "--button1" "Snooze 5m"
               "--button1-action" snooze-cmd))))))

;; ------------------------------------------------------------
;; PARSING & CHECKING
;; Supported lines:
;;   "MON 09:30 message"
;;   "09:30 message"
;; Blank lines and lines starting with ";" are ignored.
;; ------------------------------------------------------------

(defun check-tasks ()
  (multiple-value-bind (dow h m) (current-time-details)
    (let* ((now-time-str (current-time-string h m))
           (minute-key (current-minute-key dow h m)))
      (format t ".") ; heartbeat
      (force-output)

      ;; Safety: prevent multiple alerts within the same minute
      (when (string= minute-key *last-alert-key*)
        (return-from check-tasks nil))
      (setf *last-alert-key* minute-key)

      (if (probe-file *todo-path*)
          (let ((lines (read-file-lines *todo-path*)))
            (dolist (raw lines)
              (let ((line (trim-line raw)))
                (unless (or (= (length line) 0)
                            (char= (char line 0) #\;))
                  (handler-case
                      (cond
                        ;; 1) DOW HH:MM message
                        ;; Example: "MON 09:30 Pay rent"
                        ((and (>= (length line) 9)
                              (char= (char line 3) #\Space)
                              (char= (char line 6) #\:))
                         (let* ((task-dow  (subseq line 0 3))
                                (task-time (subseq line 4 9))
                                (task-msg  (if (> (length line) 10)
                                               (subseq line 10)
                                               "")))
                           (when (and (string= task-dow dow)
                                      (string= task-time now-time-str))
                             (send-alert task-msg))))

                        ;; 2) HH:MM message (daily)
                        ;; Example: "09:30 Drink water"
                        ((and (>= (length line) 5)
                              (char= (char line 2) #\:))
                         (let* ((task-time (subseq line 0 5))
                                (task-msg  (if (> (length line) 6)
                                               (subseq line 6)
                                               "")))
                           (when (string= task-time now-time-str)
                             (send-alert task-msg)))))
                    (error (c)
                      (format t "~%[ERROR PARSING] Line: ~s. Error: ~a~%"
                              raw c)
                      nil))))))
          (format t "~%[Error] File not found: ~a~%" *todo-path*)))))


;; ------------------------------------------------------------
;; MAIN LOOP
;; Align checks to minute boundaries.
;; ------------------------------------------------------------

(defun seconds-until-next-minute ()
  (multiple-value-bind (s m h d mo y dow) (get-decoded-time)
    (declare (ignore m h d mo y dow))
    (let ((remaining (- 60 s)))
      (if (= remaining 60) 0 remaining))))

(defun main ()
  (format t "Scheduler Started (DOW aware). (Ctrl+C to stop)~%")
  (format t "Todo file: ~a~%" *todo-path*)
  (format t "Snooze script: ~a~%" *snooze-script*)
  (force-output)

  ;; Optional: do one immediate check
  (check-tasks)

  (loop
    ;; Sleep until the next minute boundary
    (sleep (seconds-until-next-minute))
    (check-tasks)))

(main)