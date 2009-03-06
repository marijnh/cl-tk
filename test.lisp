(in-package :cl-tk)

(defmacro with-random-tk (&body body)
  `(with-tk ((if (zerop (random 2)) 'wish-tk 'ffi-tk)) ,@body))

(defun test-button ()
  (with-random-tk
    (let ((stop nil))
      (tcl "ttk::button" ".b" :text "Exit" :command (event-handler* (setf stop t)))
      (tcl "pack .b")
      (loop :while (alive-p)
            :do (doevent t)
            :do (when stop (destroy))))
    (mainloop)))
