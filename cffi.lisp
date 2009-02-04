(in-package :cl-tk)

(define-foreign-library tcl
  (:darwin (:framework "Tcl"))
  (:windows (:or "/tcl/bin/Tcl85.dll"))
  (:unix (:or "libtcl8.5.so" "libtcl.so"))
  (t (:default "libtcl")))

(define-foreign-library tk
  (:darwin (:framework "Tk"))
  (:windows (:or "/tcl/bin/tk85.dll"))
  (:unix (:or "libtk8.5.so" "libtk.so"))
  (t (:default "libtk")))

(eval-when (compile eval load)
  (defconstant +tcl-ok+ 0)
  (defconstant +tcl-error+ 1)
  (defconstant +tcl-dont-wait+ 2))

(defcfun ("Tcl_CreateInterp" create-interp) :pointer)
(defcfun ("Tcl_DeleteInterp" delete-interp) :void (interp :pointer))
(defcfun ("Tcl_Init" tcl-init) :int (interp :pointer))
(defcfun ("Tcl_Eval" tcl-eval) :int (interp :pointer) (script :string)) ;; TODO external-format?
(defcfun ("Tcl_GetStringResult" get-string-result) :string (interp :pointer))
(defcfun ("Tcl_DoOneEvent" do-one-event) :int (flags :int))
(defcfun ("Tk_Init" tk-init) :int (interp :pointer))

(defvar *used* nil)

(defun load-libs ()
  (unless *used*
    (handler-case (progn (use-foreign-library tcl)
                         (use-foreign-library tk)
                         (setf *used* t))
      (error (e) (tcl-error (princ-to-string e))))))

(defclass ffi-tk (tk)
  ((interp :reader @interp)
   (alive :initform nil :accessor @alive)))

(defmethod initialize-instance ((tk ffi-tk) &key &allow-other-keys)
  (load-libs)
  (let ((int (create-interp)))
    (when (zerop int) (tcl-error "Could not create interpreter."))
    (unless (and (= (tcl-init int) +tcl-ok+) (= (tk-init int) +tcl-ok+))
      (tcl-error "Initialising Tcl/Tk failed."))
    (setf (slot-value tk 'interp) int
          (@alive tk) t))
  (tcl-send tk "proc _esc {s} {format {\"%s\"} [regsub -all {\"} [regsub -all {\\\\} $s {\\\\\\\\}] {\\\"}]}")
  (tcl-send tk "set _events {}")
  (tcl-send tk "proc _ev {args} {global _events; foreach arg $args {lappend escaped [_esc $arg]}; lappend _events \"([concat escaped])\"}")
  (tcl-send tk "proc _get_ev {} {global _events; set ret [lindex $_events 0]; set _events [lrange $_events 1 end]; set ret}")
  (tk-doevents tk))

(defmethod tk-destroy ((tk ffi-tk))
  (delete-interp (@interp tk))
  (setf (@alive tk) nil))

(defmethod tcl-send ((tk ffi-tk) command &optional (get-result t))
  (unless (@alive tk) (tcl-error "Tk instance no longer alive."))
  (case (tcl-eval (@interp tk) command)
    (#.+tcl-error+ (tcl-error (get-string-result (@interp tk))))
    (#.+tcl-ok+ (when get-result (get-string-result (@interp tk))))))

(defmethod tk-doevents ((tk ffi-tk) &optional block)
  (loop :for result := (do-one-event (if block 0 +tcl-dont-wait+))
        :while (= result 1))
  (loop :for result := (tcl-send tk "_get_ev")
        :until (string= result "")
        :do (let ((expr (read-from-string result)))
              (handle-event tk (parse-integer (car expr)) (cdr expr)))))
