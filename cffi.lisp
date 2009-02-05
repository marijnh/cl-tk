(in-package :cl-tk)

(cffi:define-foreign-library tcl
  (:darwin (:framework "Tcl"))
  (:windows (:or "/tcl/bin/Tcl85.dll"))
  (:unix (:or "libtcl8.5.so" "libtcl.so"))
  (t (:default "libtcl")))

(cffi:define-foreign-library tk
  (:darwin (:framework "Tk"))
  (:windows (:or "/tcl/bin/tk85.dll"))
  (:unix (:or "libtk8.5.so" "libtk.so"))
  (t (:default "libtk")))

(let ((loaded nil))
  (defun use-libs ()
    (unless loaded
      (cffi:use-foreign-library tcl)
      (cffi:use-foreign-library tk)
      (setf loaded t))))

(eval-when (compile eval load)
  (defconstant +tcl-ok+ 0)
  (defconstant +tcl-error+ 1)
  (defconstant +tcl-dont-wait+ 2))

(cffi:defcfun ("Tcl_CreateInterp" create-interp) :pointer)
(cffi:defcfun ("Tcl_DeleteInterp" delete-interp) :void (interp :pointer))
(cffi:defcfun ("Tcl_Init" tcl-init) :int (interp :pointer))
(cffi:defcfun ("Tcl_Eval" tcl-eval) :int (interp :pointer) (script :string)) ;; TODO external-format?
(cffi:defcfun ("Tcl_GetStringResult" get-string-result) :string (interp :pointer))
(cffi:defcfun ("Tcl_DoOneEvent" do-one-event) :int (flags :int))
(cffi:defcfun ("Tk_Init" tk-init) :int (interp :pointer))

(defclass ffi-tk (tk)
  ((interp :reader @interp)
   (alive :initform nil :accessor @alive)))

(defmethod initialize-instance :after ((tk ffi-tk) &key &allow-other-keys)
  (handler-case (use-libs)
    (error (e) (tcl-error (princ-to-string e))))
  (let ((int (create-interp)))
    (when (zerop int) (tcl-error "Could not create interpreter."))
    (unless (and (= (tcl-init int) +tcl-ok+) (= (tk-init int) +tcl-ok+))
      (tcl-error "Initialising Tcl/Tk failed."))
    (setf (slot-value tk 'interp) int
          (@alive tk) t))
  (tcl-send tk "proc _esc {s} {format {\"%s\"} [regsub -all {\"} [regsub -all {\\\\} $s {\\\\\\\\}] {\\\"}]}")
  (tcl-send tk "set _events {}")
  (tcl-send tk "proc _ev {args} {global _events; foreach arg $args {append escaped [_esc $arg]}; lappend _events \"([concat $escaped])\"}")
  (tcl-send tk "proc _get_ev {} {global _events; set ret [lindex $_events 0]; set _events [lrange $_events 1 end]; set ret}")
  (let ((*tk* tk))
    (bind-event "." "<Destroy>" () (tk-destroy tk))
    (doevents)))

(defmethod tk-destroy ((tk ffi-tk))
  (when (@alive tk)
    (delete-interp (@interp tk))
    (setf (@alive tk) nil)))

(defmethod tk-alive-p ((tk ffi-tk))
  (@alive tk))

(defmethod tcl-send ((tk ffi-tk) command &optional (get-result t))
  (unless (@alive tk) (tcl-error "Tk instance no longer alive."))
  (case (tcl-eval (@interp tk) command)
    (#.+tcl-error+ (tcl-error (get-string-result (@interp tk))))
    (#.+tcl-ok+ (when get-result (get-string-result (@interp tk))))))

(defmethod tk-doevent ((tk ffi-tk) &optional block)
  (unless (@alive tk) (return-from tk-doevent nil))
  (when (= (do-one-event (if block 0 +tcl-dont-wait+)) 1)
    (loop :for result := (tcl-send tk "_get_ev")
          :until (string= result "")
          :do (let ((expr (read-from-string result)))
                (handle-event tk (parse-integer (car expr)) (cdr expr)))
          :unless (@alive tk) :do (return))
    t))
