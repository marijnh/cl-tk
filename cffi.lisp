(in-package :cl-tk)

(cffi:define-foreign-library tcl
  (:darwin (:framework "Tcl"))
  (:windows (:or "tcl85.dll"))
  (:unix (:or "libtcl8.5.so" "libtcl.so"))
  (t (:default "libtcl")))

(cffi:define-foreign-library tk
  (:darwin (:framework "Tk"))
  (:windows (:or "tk85.dll"))
  (:unix (:or "libtk8.5.so" "libtk.so"))
  (t (:default "libtk")))

(let ((loaded nil))
  (defun load-libs ()
    (unless loaded
      (cffi:use-foreign-library tcl)
      (cffi:use-foreign-library tk)
      (setf loaded t))))

(cffi:defcfun ("Tcl_CreateInterp" create-interp) :pointer)
(cffi:defcfun ("Tcl_DeleteInterp" delete-interp) :void (interp :pointer))
(cffi:defcfun ("Tcl_Init" tcl-init) :int (interp :pointer))
(cffi:defcfun ("Tcl_Eval" tcl-eval) :int (interp :pointer) (script (:string :encoding :utf-8)))
(cffi:defcfun ("Tcl_GetStringResult" get-string-result) (:string :encoding :utf-8) (interp :pointer))
(cffi:defcfun ("Tcl_DoOneEvent" do-one-event) :int (flags :int))
(cffi:defcfun ("Tk_Init" tk-init) :int (interp :pointer))

(defun null-pointer-p (ptr) (cffi:null-pointer-p ptr))
