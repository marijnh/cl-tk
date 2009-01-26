(in-package :cl-tk)

(defun wish-stream (binary-name)
  (or
   #+(or cmu scl)
   (let ((proc (run-program binary-name nil :input :stream :output :stream :wait nil
                            #+scl :external-format #+scl :utf-8)))
     (when proc
       (make-two-way-stream
        (ext:process-output proc)
        (ext:process-input proc))))

   #+clisp
   (ext:run-program binary-name :input :stream :output :stream :wait nil)

   #+sbcl
   (let ((proc (sb-ext:run-program binary-name nil :input :stream :output :stream :wait nil :search t)))
     (when proc
       #+:ext-8859-1
       (make-two-way-stream
        (sb-sys:make-fd-stream (sb-sys:fd-stream-fd (process-output proc))
                               :input t :external-format :iso-8859-1)
        (sb-sys:make-fd-stream (sb-sys:fd-stream-fd (process-input proc))
                               :output t  :external-format :iso-8859-1))
       #-:ext-8859-1
       (make-two-way-stream (sb-ext:process-output proc) (sb-ext:process-input proc))))

   #+lispworks
   (system:open-pipe fullstring :direction :io)

   #+allegro
   (excl:run-shell-command binary-name :input :stream :output :stream :wait nil)

   #+ecl
   (ext:run-program binary-name nil :input :stream :output :stream :error :output)

   #+openmcl
   (let ((proc (ccl:run-program binary-name nil :input :stream :output :stream :wait nil)))
     (when proc
       (make-two-way-stream (ccl:external-process-output-stream proc)
                            (ccl:external-process-input-stream proc))))

   (error 'tk-error :format-control "Could not start wish process.")))

(defparameter *default-binary* "wish")

(defvar *wish*)

(defclass wish ()
  ((stream :initarg :stream :reader @stream)
   (queue :initform () :accessor @queue)
   (event-table :initform (make-hash-table :test 'equal) :reader @table)))

(defun start-wish (binary)
  (let ((*wish* (make-instance 'wish :stream (wish-stream binary))))
    (init-wish)
    *wish*))

(defun wish-toplevel (&optional (binary *default-binary*))
  (setf *wish* (start-wish binary)))

(defmacro with-wish ((&optional (binary *default-binary*)) &body body)
  `(let ((*wish* (start-wish ,binary)))
     ,@body))

(defun write-wish (str)
  (let ((stream (@stream *wish*)))
    (write-string str stream)
    (write-char #\newline stream)
    (finish-output stream)))

(defun init-wish ()
  (write-wish "package require Tk")
  (write-wish "proc esc {s} {format {\"%s\"} [regsub -all {\"} [regsub -all {\\\\} $s {\\\\\\\\}] {\\\"}]}")
  (write-wish "proc lst {type args} {puts \"(:$type\"; foreach arg $args {puts \" [esc $arg]\";}; puts \")\\n\"; flush stdout}")
  (write-wish "proc ev {args} {lst e $args}")
  (write-wish "proc run {args} {set res {}; set err {}; if [catch {set res [eval $args]} err] {lst x $err} {lst d $res}}"))

(defun register-event (id handler)
  (setf (gethash id (@table *wish*)) handler))
(defun unregister-event (id)
  (remhash id (@table *wish*)))