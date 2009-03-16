(defpackage :cl-tk
  (:use :cl)
  (:export #:tcl-error
           #:toplevel-tk #:with-tk #:*tk*
           #:tcl-escape #:lit #:tcl #:tcl[ #:tcl{
           #:wname-cons #:wname-car #:wname-cdr
           #:with-wname #:wname #:*wname*
           #:event-handler #:event-handler* #:unregister-event
           #:bind-event
           #:with-local-events #:event-snapshot #:clear-events
           #:doevent #:doevents #:mainloop
           #:wish-tk #:ffi-tk #:destroy #:alive-p))
