(in-package :cl-tk)

(let ((loaded nil))
  (defun load-libs ()
    (unless loaded
      #+unix
      (progn (load "libtcl8.5.so")
             (load "libtk8.5.so"))
      #+mswindows
      (progn (load "tcl85.dll")
             (load "tk85.dll"))
      #+(and (not mswindows) (not unix))
      (error "Don't know how to load Tk libraries on this system.")
      (setf loaded t))))

(ff:def-foreign-call (create-interp "Tcl_CreateInterp") (:void) :returning (:foreign-address))
(ff:def-foreign-call (delete-interp "Tcl_DeleteInterp") ((interp :foreign-address)) :returning :void)
(ff:def-foreign-call (tcl-init "Tcl_Init") ((interp :foreign-address)) :returning (:int))
(ff:def-foreign-call (tcl-eval% "Tcl_Eval") ((interp :foreign-address) (script :foreign-address)) :returning (:int))
(defun tcl-eval (interp script)
  (excl:with-native-string (script script :external-format :utf-8)
    (tcl-eval% interp script)))
(ff:def-foreign-call (get-string-result% "Tcl_GetStringResult") ((interp :foreign-address)) :returning (:foreign-address))
(defun get-string-result (interp)
  (excl:native-to-string (get-string-result% interp) :external-format :utf-8))
(ff:def-foreign-call (do-one-event "Tcl_DoOneEvent") ((flags :int)) :returning (:int))
(ff:def-foreign-call (tk-init "Tk_Init") ((interp :foreign-address)) :returning (:int))

(defun null-pointer-p (ptr) (zerop ptr))
