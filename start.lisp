(in-package :cl-tk)

(defun start-tk (&optional class)
  (if class
      (make-instance class)
      (handler-case (make-instance 'cffi-tk)
        (error () (make-instance 'wish-tk)))))

(defmacro with-tk ((&optional class) &body body)
  `(let ((*tk* (start-tk ,class))) ,@body))

(defun toplevel-tk (&optional class)
  (setf *tk* (start-tk class)))
