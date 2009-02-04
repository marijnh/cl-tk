(in-package :cl-tk)

(defun test-button ()
  (let ((stop nil))
    (tcl "ttk::button" ".b" :text "Exit" :command (event-handler (lambda () (setf stop t))))
    (tcl "pack .b")
    (loop :while (alive-p)
          :do (doevent t)
          :do (when stop (destroy)))))
