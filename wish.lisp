(in-package :cl-tk)

;; Some inspiration taken from Peter Herth's LTK project: http://www.peter-herth.de/ltk/

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

   (tcl-error "Could not start wish process.")))

(defparameter *wish-binary* "wish")

(defclass wish-tk (tk)
  ((stream :reader @stream)
   (queue :initform () :accessor @queue)))

(defmethod initialize-instance ((tk wish-tk) &key &allow-other-keys)
  (setf (slot-value tk 'stream) (wish-stream *wish-binary*))
  (tcl-send tk "package require Tk 8.5" nil)
  (tcl-send tk "proc _esc {s} {format {\"%s\"} [regsub -all {\"} [regsub -all {\\\\} $s {\\\\\\\\}] {\\\"}]}" nil)
  (tcl-send tk "proc _lst {type args} {puts \"(:$type\"; foreach arg $args {puts \" [_esc $arg]\"}; puts \")\\n\"; flush stdout}" nil)
  (tcl-send tk "proc _ev {args} {_lst e {*}$args}" nil)
  (tcl-send tk "proc _run {stat} {if [catch {set res [uplevel #0 $stat]} err] {_lst x $err} {_lst d $res}}" nil))

(defmethod tk-destroy ((tk wish-tk))
  (when (open-stream-p (@stream tk))
    (tcl-send tk "destroy .")))

(defun read-wish-message (stream &optional type)
  (unless (eql (peek-char t stream) #\()
    (let ((junk (with-output-to-string (out)
                  (loop :for ch := (read-char-no-hang stream)
                        :if (not ch) :do (return)
                        :if (eql ch #\() :do (unread-char ch stream)
                        :and :do (return)
                        :do (write-char ch out)))))
      (tcl-error "Junk in wish output: '~a'" junk)))
  (let ((expr (read-preserving-whitespace stream)))
    (unless (or (null type) (eq (car expr) type))
      (tcl-error "Unexpected message '~a' from wish." (car expr)))
    (values (cdr expr) (car expr))))

(defmethod tcl-send ((tk wish-tk) command &optional (get-result nil))
  (let ((stream (@stream tk)))
    (format stream (if get-result "_run {~a}~%" "~a~%") command)
    (finish-output stream)
    (when get-result
      (loop :for (val type) := (multiple-value-list (read-wish-message stream))
            :do (ecase type
                  (:d (return (car val)))
                  (:x (tcl-error (car val)))
                  (:e (setf (@queue tk) (append (@queue tk) (list val)))))))))

(defun input-pending-p (stream)
  (loop :for ch := (read-char-no-hang stream)
        :if (not ch) :do (return nil)
        :if (graphic-char-p ch) :do (progn (unread-char ch stream)
                                           (return t))))

(defmethod tk-doevents ((tk wish-tk) &optional block)
  (let ((stream (@stream tk)))
    (handler-case
        (loop :while (or block (input-pending-p stream))
              :do (destructuring-bind (id &rest args) (read-wish-message stream :e)
                    (handle-event tk (parse-integer id) args)))
      (end-of-file () nil))))
