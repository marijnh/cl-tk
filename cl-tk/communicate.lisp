(in-package :cl-tk)

(defun read-wish-message (&optional type)
  (let ((expr (read-preserving-whitespace (@stream *wish*))))
    (when type
      (tk-assert (eq (car expr) type) "Unexpected message '~a' from wish." (car expr)))
    (values (cdr expr) (car expr))))

(defun eval-wish (&rest strings)
  (wformat "run {~{~a~^ ~}}" strings)
  (loop :for (val type) := (multiple-value-list (read-wish-message))
        :do (ecase type
              (:d (return (car val)))
              (:x (error 'tk-error :format-control val))
              (:e (setf (@queue *wish*) (append (@queue *wish*) (list val)))))))

(defun maybe-handle-event ()
  (let ((stream (@stream *wish*)))
    (loop :for ch := (read-char-no-hang stream)
          :while (and ch (not (graphic-char-p ch)))
          :finally (when ch
                     (unread-char ch stream)
                     (handle-event)
                     (return t)))))

(defun handle-event ()
  (let* ((ev (if (@queue *wish*)
                 (pop (@queue *wish*))
                 (read-wish-message :e)))
         (handler (gethash (parse-integer (car ev)) (@table *wish*))))
    (if handler
        (apply handler (cdr ev))
        (warn "Event '~a' fired, but no handler exists." (car ev)))))

(defun main-loop ()
  (loop (handler-case (handle-event)
          (end-of-file () (return)))))

(defun event-handler (func fields)
  (let ((id (register-event func)))
    (values (format nil "{ev ~a~{ %~a~}}" id fields) id)))

(defmacro bind-event (tag event (&rest fields) &body body)
  "For example (bind-event \".\" \"<1>\" ((x #\x) (y #\y)) (format t \"clicked ~a,~a\" x y))"
  `(wformat "bind ~a ~a ~a" ,tag ,event
            (event-handler (lambda ,(mapcar #'first fields) ,@body) ',(mapcar #'second fields))))
