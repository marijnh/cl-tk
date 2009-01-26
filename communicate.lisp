(in-package :cl-tk)

(defun read-wish-message (&optional assert-type)
  (let* ((stream (@stream *wish*))
         (type (read-char stream)))
    (when assert-type (tk-assert (eql type assert-type) "Unexpected message '~a' from wish." type))
    (tk-assert (eql (read-char stream) #\space) "Wish connection corrupted -- expected space.")
    (let ((str (read-preserving-whitespace stream)))
      (tk-assert (eql (read-char stream) #\newline) "Wish connection corrupted -- expected newline.")
      (values str type))))

(defun read-event ()
  (let ((ev (if (@queue *wish*)
                (pop (@queue *wish*))
                (read-wish-message #\e))))
    ev)) ;; TODO Handle event

(defun eval-wish (string)
  (write-wish (concatenate 'string "run " string))
  (loop :for (val type) := (multiple-value-list (read-wish-message))
        :do (ecase type
              (#\d (return val))
              (#\x (error 'tk-error :format-control val))
              (#\e (setf (@queue *wish*) (append (@queue *wish*) (list val)))))))
