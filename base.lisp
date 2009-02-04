(in-package :cl-tk)

(define-condition tcl-error (simple-error) ())
(defun tcl-error (control &rest args)
  (error 'tcl-error :format-control control :format-arguments args))

(defvar *tk*)

(defclass tk ()
  ((event-table :initform (make-hash-table :test 'eql) :reader @table)
   (next-id :initform 0 :accessor @next-id)))

(defun register-event (handler)
  (let ((id (incf (@next-id *tk*))))
    (setf (gethash id (@table *tk*)) handler)
    id))

(defun unregister-event (id)
  (remhash id (@table *tk*)))

(defun event-handler (func &optional (fields ()))
  (let ((id (register-event func)))
    (values (format nil "_ev ~a~{ %~a~}" id fields) id)))

(defmacro bind-event (tag event (&rest fields) &body body)
  "For example (bind-event \".\" \"<1>\" ((x #\x) (y #\y)) (format t \"clicked ~a,~a\" x y))"
  `(tcl "bind" ,tag ,event
        (tcl{ (event-handler (lambda ,(mapcar #'first fields) ,@body) ',(mapcar #'second fields)))))

(defun handle-event (tk id args)
  (let ((handler (gethash id (@table tk))))
    (if handler
        (apply handler args)
        (warn "Event '~a' fired, but no handler exists." id))))

(defgeneric tk-destroy (tk))
(defun destroy () (tk-destroy *tk*))
(defgeneric tk-doevents (tk &optional block))
(defun doevents (&optional block) (tk-doevents *tk* block))
(defgeneric tcl-send (tk command &optional get-result))

(defun tcl-escape (str)
  (with-output-to-string (out)
    (write-char #\" out)
    (loop :for ch :across str
          :do (princ (case ch
                       (#\newline "\\n") (#\tab "\\t") (#\backspace "\\b")
                       (#\page "\\f") (#\return "\\r") (#\vt "\\v") (#\bell "\\a")
                       ((#\" #\\ #\[ #\$) (princ #\\ out) ch)
                       (t ch)) out))
    (write-char #\" out)))

(defstruct (literal-string (:constructor lit (val))) val)

(defun tcl-form (val)
  (etypecase val
    (keyword (format nil "-~a" (string-downcase (symbol-name val))))
    (string (tcl-escape val))
    (number (princ-to-string val))
    (literal-string (literal-string-val val))
    (list (format nil "~{~a~^ ~}" (mapcar #'tcl-form val)))))

(flet ((as-string (before after command args)
         (format nil "~a~a~{ ~a~}~a" before command (mapcar 'tcl-form args) after)))
  (defun tcl[ (command &rest args)
    (as-string #\[ #\] command args))
  (defun tcl{ (command &rest args)
    (as-string #\{ #\} command args))
  (defun tcl (command &rest args)
    (tcl-send *tk* (as-string "" "" command args))))
