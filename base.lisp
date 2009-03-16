(in-package :cl-tk)

(define-condition tcl-error (simple-error) ())
(defun tcl-error (control &rest args)
  (error 'tcl-error :format-control control :format-arguments args))

(defvar *tk*)

(defclass tk ()
  ((event-table :initform (make-hash-table :test 'eql) :reader @table)
   (next-id :initform 0 :accessor @next-id)))

;; Event callback registration

(defun register-event (handler)
  (let ((id (@next-id *tk*)))
    (incf (@next-id *tk*))
    (setf (gethash id (@table *tk*)) handler)
    id))

(defun unregister-event (id)
  (remhash id (@table *tk*)))

(defun event-handler (func &optional (fields ()))
  (let ((id (register-event func)))
    (values (format nil "_ev ~a~{ %~a~}" id fields) id)))

(defmacro event-handler* (&body body)
  `(event-handler (lambda () ,@body)))

(defmacro bind-event (tag event (&rest fields) &body body)
  "For example (bind-event \".\" \"<1>\" ((x #\x) (y #\y)) (format t \"clicked ~a,~a\" x y))"
  `(multiple-value-bind (handler id) (event-handler (lambda ,(mapcar #'first fields) ,@body) ',(mapcar #'second fields))
     (tcl "bind" ,tag ,event handler)
     id))

(defun handle-event (tk id args)
  (let ((handler (gethash id (@table tk))))
    (if handler
        (apply handler args)
        (warn "Event '~a' fired, but no handler exists." id))))

(defun event-snapshot ()
  (@next-id *tk*))
(defun clear-events (snapshot)
  (let ((tab (@table *tk*)))
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (>= k snapshot) (remhash k tab)))
             tab)
    (setf (@next-id *tk*) snapshot)))

(defmacro with-local-events (&body body)
  (let ((snap (gensym)))
    `(let ((,snap (event-snapshot)))
       (unwind-protect (progn ,@body)
         (clear-events ,snap)))))

;; Methods on tk objects

(defgeneric tk-destroy (tk))
(defgeneric tk-alive-p (tk))
(defun destroy () (tk-destroy *tk*))
(defun alive-p () (tk-alive-p *tk*))

(defgeneric tk-doevent (tk &optional block))
(defun doevent (&optional block) (tk-doevent *tk* block))
(defun doevents () (loop :while (doevent)))
(defun mainloop ()
  (loop :while (alive-p)
        :do (doevent t)))

(defgeneric tcl-send (tk command &optional get-result))

;; Tcl commands

(defun tcl-escape (str)
  (if (string= str "")
      "{}"
      (with-output-to-string (out)
        (loop :for ch :across str
              :do (princ (case ch
                           (#\newline "\\n") (#\tab "\\t") (#\backspace "\\b")
                           (#\page "\\f") (#\return "\\r") (#\vt "\\v") (#\bell "\\a")
                           ((#\" #\\ #\[ #\] #\$ #\space) (princ #\\ out) ch)
                           (t ch)) out)))))

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
    (lit (as-string #\[ #\] command args)))
  (defun tcl{ (command &rest args)
    (lit (as-string #\{ #\} command args)))
  (defun tcl (command &rest args)
    (tcl-send *tk* (as-string "" "" command args))))

;; wnames

(defun wname-cons (base name)
  (format nil "~a.~a" (if (string= base ".") "" base) name))
(flet ((find-dot (name)
         (or (position #\. name :from-end t)
             (tcl-error "~a is not a valid wname" name))))
  (defun wname-car (name)
    (subseq name (1+ (find-dot name))))
  (defun wname-cdr (name)
    (subseq name 0 (max 1 (find-dot name)))))

(defvar *wname* ".")
(defmacro with-wname (name &body body)
  `(let ((*wname* ,name)) ,@body))
(defun wname (name &optional id)
  (wname-cons *wname* (if id (format nil "~a~a" name id) name)))

;; Running a Tk instance

(defun start-tk (&optional back-end)
  (or back-end
      (if (find-class 'ffi-tk)
          (handler-case (make-instance 'ffi-tk)
            (error (e) (warn "Failed to start FFI back-end: ~a" (princ-to-string e))
                       (make-instance 'wish-tk)))
          (make-instance 'wish-tk))))

(defmacro with-tk ((&optional back-end) &body body)
  `(let ((*tk* (start-tk ,back-end)))
     (unwind-protect (progn ,@body)
       (destroy))))

(defun toplevel-tk (&optional back-end)
  (setf *tk* (start-tk back-end)))
