(in-package :cl-tk)

(defun read-wish-message (&optional type)
  (let ((expr (read-preserving-whitespace (@stream *wish*))))
    (when type
      (tk-assert (eq (car expr) type) "Unexpected message '~a' from wish." (car expr)))
    (values (cdr expr) (car expr))))

(defun eval-wish (string)
  (write-wish (concatenate 'string "run " string))
  (loop :for (val type) := (multiple-value-list (read-wish-message))
        :do (ecase type
              (:d (return (car val)))
              (:x (error 'tk-error :format-control val))
              (:e (setf (@queue *wish*) (append (@queue *wish*) (list val)))))))

(defun handle-event ()
  (let* ((ev (if (@queue *wish*)
                 (pop (@queue *wish*))
                 (read-wish-message #\e)))
         (handler (gethash (car ev) (@table *wish*))))
    (if handler
        (apply handler (cdr ev))
        (warn "Event '~a' fired, but no handler exists." (car ev)))))

(defun main-loop ()
  (loop (handler-case (handle-event)
          (end-of-file () (return)))))
