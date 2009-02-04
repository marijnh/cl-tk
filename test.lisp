(in-package :cl-tk)

(defun test-button ()
  (let ((stop nil))
    (tcl "ttk::button" ".b" :text "Exit" :command (event-handler (lambda () (setf stop t))))
    (tcl "pack .b")
    (loop :while (alive-p)
          :do (doevent t)
          :do (when stop (destroy)))))

(defun test-grid ()
  (flet ((label (n col)
           (tcl[ "ttk::label" n :text n :background col))
         (button (n)
           (tcl[ "ttk::button" n :text n)))
    (build-grid
      (((button ".left") :rowspan 3 :sticky "news")
       ((label ".top" "#f00") :sticky "news")
       ((label ".topright" "#0f0") :sticky "news"))
      (nil
       ((label ".centerright" "#00f") :columnspan 2 :sticky "news"))
      (nil
       nil
       ((button ".bottomright") :sticky "news"))))
  (mainloop))
