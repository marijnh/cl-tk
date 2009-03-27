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

(defclass wish-tk (tk)
  ((stream :reader @stream)
   (binary :initarg :binary :initform '("wish8.5" "wish85" "wish") :reader @binary)
   (queue :initform () :accessor @queue)
   (alive :initform t :accessor @alive)))

(defun try-wish-binaries (binaries)
  (flet ((try-stream (stream)
           (format stream "puts {test}~%" stream)
           (finish-output stream)
           (unless (string= (read-line stream) "test") (error "NOPE"))))
    (dolist (binary binaries)
      (let ((stream (ignore-errors (wish-stream binary))))
        (when stream
          (handler-case (progn (try-stream stream)
                               (return stream))
            (error ())))))))

(defmethod initialize-instance :after ((tk wish-tk) &key &allow-other-keys)
  (let ((binaries (if (consp (@binary tk)) (@binary tk) (list (@binary tk)))))
    (setf (slot-value tk 'stream)
          (or (try-wish-binaries binaries)
              (tcl-error "Could not open Wish shell (tried ~{'~a'~^, ~})" binaries))))
  (tcl-send tk "package require Tk 8.5" nil)
  (tcl-send tk "proc _esc {s} {format {\"%s\"} [regsub -all {\"} [regsub -all {\\\\} $s {\\\\\\\\}] {\\\"}]}" nil)
  (tcl-send tk "proc _lst {type args} {puts \"(:$type\"; foreach arg $args {puts \" [_esc $arg]\"}; puts \")\\n\"; flush stdout}" nil)
  (tcl-send tk "proc _ev {args} {_lst e {*}$args}" nil)
  (tcl-send tk "proc _run {stat} {if [catch {set res [uplevel #0 $stat]} err] {_lst x $err} {_lst d $res}}" nil)
  (tcl-send tk "puts {}" nil)
  (let ((out (read-line (@stream tk))))
    (unless (string= out "")
      (if (and (> (length out) 33) (string= (subseq out 0 33) "version conflict for package \"Tk\""))
          (tcl-error "Incorrect Tcl/Tk version: ~a" out)
          (tcl-error "Tcl started with an unexpected message: ~a" out)))))

(defmethod tk-destroy ((tk wish-tk))
  (when (@alive tk)
    (ignore-errors (tcl-send tk "destroy ."))))

(defmethod tk-alive-p ((tk wish-tk))
  (@alive tk))

(defun read-wish-message (stream &optional type)
  (unless (eql (peek-char t stream) #\()
    (let ((junk (with-output-to-string (out)
                  (loop :for ch := (read-char-no-hang stream) :while ch
                        :if (eql ch #\() :do (progn (unread-char ch stream) (return))
                        :do (write-char ch out)))))
      (tcl-error "Junk in wish output: '~a'" junk)))
  (let ((expr (read-preserving-whitespace stream)))
    (unless (or (null type) (eq (car expr) type))
      (tcl-error "Unexpected message '~a' from wish." (car expr)))
    (values (cdr expr) (car expr))))

(defmethod tcl-send ((tk wish-tk) command &optional (get-result t))
  (let ((stream (@stream tk)))
    (handler-case (format stream (if get-result "_run {~a}~%" "~a~%") command)
      (error ()
        (setf (@alive tk) nil)
        (tcl-error "Wish process exited.")))
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

(defmethod tk-doevent ((tk wish-tk) &optional block)
  (let ((stream (@stream tk)))
    (handler-case
        (when (or block (input-pending-p stream))
          (destructuring-bind (id &rest args) (read-wish-message stream :e)
            (handle-event tk (parse-integer id) args))
          t)
      (end-of-file () (setf (@alive tk) nil)))))
