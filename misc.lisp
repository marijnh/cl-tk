(in-package :cl-tk)

(define-condition tk-error (simple-error) ())
(defun tk-assert (val message &rest args)
  (unless val (error 'tk-error :format-control message :format-arguments args)))