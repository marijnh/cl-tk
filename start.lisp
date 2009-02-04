(in-package :cl-tk)

(defun tk-obj ()
  (handler-case (make-instance 'cffi-tk)
    (error () (make-instance 'wish-tk))))

(defmacro with-tk (&body body)
  `(let ((*tk* (tk-obj))) ,@body))

(defun toplevel-tk ()
  (setf *tk* (tk-obj)))
